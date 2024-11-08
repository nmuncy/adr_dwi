"""Title.

TODO

"""

import os
import re
import pandas as pd
import openpyxl  # noqa: F401
import numpy as np
from adr_dwi import database
from adr_dwi import helper

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


class SetupDb:
    """Title.

    Example:
        setup_db = workflows.SetupDb()
        setup_db.load_data()
        setup_db.make_part_tables()

    """

    def __init__(self):
        """Initialize SetupDb."""
        log.write.info("Initializing SetupDb")
        self._db_con = database.DbConnect()
        self._ref_maps = database.RefMaps(self._db_con)
        self._data_dir = os.path.join(os.environ["HOME"], "Projects", "data")

    def load_data(self):
        """Title."""
        log.write.info("Loading data")
        raw_demo = os.path.join(
            self._data_dir, "participant_list_for_nate.xlsx"
        )
        raw_impact = os.path.join(self._data_dir, "impact_for_nate_clean.xlsx")
        for chk_path in [raw_demo, raw_impact]:
            if not os.path.exists(chk_path):
                raise FileNotFoundError(chk_path)

        self._df_part = pd.read_excel(raw_demo)
        self._df_clean = pd.read_excel(raw_impact, "v1_clean")

    def _clean_part(self):
        """Title."""
        log.write.info("Cleaning participant data")
        self._df_part = self._df_part[
            self._df_part["SubjectID_baseline"].notna()
        ]
        self._df_part.columns = self._df_part.columns.str.replace("#", "")
        self._df_part.columns = self._df_part.columns.str.replace("-", "_")
        self._df_part.columns = self._df_part.columns.str.replace(" ", "_")

    def make_part_tables(self):
        """Title."""
        self._clean_part()
        self._ref_subj()
        self._tbl_demo()
        self._scan_date()

    def _ref_subj(self):
        """Title."""
        log.write.info("Making ref_subj")
        self._df_part["subj_id"] = (
            self._df_part["SubjectID_baseline"].str[3:7].astype(int)
        )
        self._df_part["subj_name"] = self._df_part["SubjectID_baseline"].str[
            3:7
        ]
        self._df_part["subj_sky_base"] = self._df_part["Baseline_SKY_"]
        self._df_part["subj_sky_fu1"] = self._df_part["Follow_up_1_SKY_"]
        self._df_part["subj_sky_fu2"] = self._df_part["Follow_up_2_SKY_"]
        self._df_part["subj_sky_twin"] = self._df_part["Twin_SKY_"]

        #
        col_sub = [
            "subj_id",
            "subj_name",
            "subj_sky_base",
            "subj_sky_fu1",
            "subj_sky_fu2",
            "subj_sky_twin",
        ]
        df_subj = self._df_part[col_sub].copy()
        df_subj = df_subj.replace(np.nan, None)

        #
        col_new = {
            x: x.split("_")[-1]
            for x in col_sub
            if x not in ["subj_id", "subj_name"]
        }
        df_subj = df_subj.rename(columns=col_new)
        col_list = [y for x, y in col_new.items()]

        #
        df_long = pd.melt(
            df_subj,
            id_vars=["subj_id", "subj_name"],
            value_vars=col_list,
            var_name="sky_type",
            value_name="sky_name",
            ignore_index=False,
        )
        df_long = df_long.dropna()
        df_long["sky_type"] = df_long.apply(
            lambda x: self._ref_maps.get_id("test", x, "sky_type"), axis=1
        )

        #
        database.build_participant_tables(
            "ref_subj",
            df_long,
            ["subj_id", "subj_name", "sky_type", "sky_name"],
            self._db_con,
        )

    def _tbl_demo(self):
        """Title."""
        log.write.info("Making tbl_demo")
        self._df_part["sex"] = self._df_part["Gender"].str[:1]
        self._df_part["age_base"] = self._df_part["Age_at_Baseline"].astype(
            int
        )

        #
        col_list = ["subj_id", "sex", "age_base"]
        df_demo = self._df_part[col_list].copy()
        df_demo = df_demo.replace(np.nan, None)
        database.build_participant_tables(
            "tbl_demo", df_demo, col_list, self._db_con
        )

    def _scan_date(self):
        """Title."""
        log.write.info("Making tbl_scan_date")
        self._df_part["base"] = self._df_part["Date_of_Baseline"]
        self._df_part["fu1"] = self._df_part["Date_of_Follow_up_1"]
        self._df_part["fu2"] = self._df_part["Date_of_Follow_up_2"]

        #
        df_scan = self._df_part[["subj_id", "base", "fu1", "fu2"]].copy()
        df_long = pd.melt(
            df_scan,
            id_vars=["subj_id"],
            value_vars=["base", "fu1", "fu2"],
            var_name="scan_name",
            value_name="scan_date",
            ignore_index=False,
        )
        df_long = df_long.dropna()

        #
        df_long["scan_id"] = df_long.apply(
            lambda x: self._ref_maps.get_id("test", x, "scan_name"), axis=1
        )
        df_long["scan_date"] = df_long["scan_date"].astype(str)
        df_long["scan_date"] = df_long["scan_date"].str[:10]

        database.build_participant_tables(
            "tbl_scan_dates",
            df_long,
            ["subj_id", "scan_id", "scan_date"],
            self._db_con,
        )

    def make_impact_tables(self):
        """Title."""
        self._clean_impact()
        for self._test_type in self._test_type_list:
            self._user_data()
            self._word_data()

    def _clean_impact(self):
        """Title."""
        log.write.info("Cleaning impact data")
        #
        switch_test = {
            "Baseline": "base",
            "Post-Injury 1": "fu1",
            "Post-Injury 2": "fu2",
            "Post-Injury 3": "fu3",
            "Post-Injury 4": "fu4",
        }
        self._df_clean["testType"] = self._df_clean["testType"].map(
            switch_test
        )
        self._test_type_list = [x for _, x in switch_test.items()]

        #
        self._df_clean["test_id"] = self._df_clean.apply(
            lambda x: self._ref_maps.get_id("test", x, "testType"), axis=1
        )

        # Ugly inner func because the lambdas were breaking
        # TODO refactor
        def get_subj_id(sky_name: str, sky_type: int) -> int:
            """Return subj_id given SKY ID and type."""
            sky_name = str(sky_name)
            sky_type = int(sky_type)
            subj_id = self._ref_maps._df_subj.loc[
                (self._ref_maps._df_subj["sky_name"] == sky_name)
                & (self._ref_maps._df_subj["sky_type"] == sky_type)
            ]["subj_id"]
            return int(subj_id)

        # Get subj_id from baseline IDs
        idx_base_id = self._df_clean.index[
            (self._df_clean["base_id"] != "not_collected")
            & (self._df_clean["test_id"] == 0)
        ].to_list()
        for idx in idx_base_id:
            sky_name = self._df_clean.loc[idx, "base_id"]
            sky_type = self._df_clean.loc[idx, "test_id"]
            self._df_clean.loc[idx, "subj_id"] = get_subj_id(
                sky_name, sky_type
            )

        # Get subj_id from post IDs when no baseline available
        idx_post_id = self._df_clean.index[
            (self._df_clean["base_id"] == "not_collected")
            & (self._df_clean["post_id"] != "not_collected")
            & (self._df_clean["test_id"] == 1)
        ].to_list()
        for idx in idx_post_id:
            sky_name = self._df_clean.loc[idx, "post_id"]
            sky_type = self._df_clean.loc[idx, "test_id"]
            self._df_clean.loc[idx, "subj_id"] = get_subj_id(
                sky_name, sky_type
            )

        # Forward fill subj_ids, manage type
        self._df_clean["subj_id"] = self._df_clean["subj_id"].ffill(axis=0)
        self._df_clean["subj_id"] = self._df_clean["subj_id"].astype("Int64")

    def _user_data(self):
        """Title."""
        #
        log.write.info(
            f"Building tbl_impact_user for visit: {self._test_type}"
        )

        # Extract identifier and userItem columns
        col_user = [x for x in self._df_clean.columns if "user" in x]
        col_user = [
            "subj_id",
            "base_id",
            "post_id",
            "rtp_id",
            "tbi_num",
            "testType",
            "test_id",
            "testDate",
        ] + col_user
        idx_type = self._df_clean.index[
            self._df_clean["testType"] == self._test_type
        ].tolist()
        df_user = self._df_clean.loc[idx_type, col_user].copy()

        #
        df_user = df_user.rename(
            columns={"testDate": "impact_date", "tbi_num": "num_tbi"}
        )
        database.build_participant_tables(
            "tbl_impact_dates",
            df_user,
            ["subj_id", "test_id", "num_tbi", "impact_date"],
            self._db_con,
        )

        # Make symptom delayed int
        delay_col = [x for x in df_user.columns if "Delayed" in x]
        df_user[delay_col] = df_user[delay_col].astype("Int64")
        new_col = {
            x: f"symp_delayed_{re.findall(r'\d+', x)[0]}" for x in delay_col
        }
        df_user = df_user.rename(columns=new_col)

        # Update Symptom cols
        symp_col = [x for x in df_user.columns if "userSymptom" in x]
        new_col = {x: f"symp_{re.findall(r'\d+', x)[0]}" for x in symp_col}
        df_user = df_user.rename(columns=new_col)

        #
        new_col = {
            "userMemoryCompositeScoreVerbal": "comp_verbal_memory",
            "userMemoryCompositeScoreVisual": "comp_visual_memory",
            "userVisualMotorCompositeScore": "comp_visual_motor",
            "userReactionTimeCompositeScore": "comp_react_time",
            "userImpulseControlCompositeScore": "comp_impulse_ctrl",
            "userTotalSymptomScore": "symp_total_score",
            "userHoursOfSleepLastNight": "num_hours_slept",
        }
        df_user = df_user.rename(columns=new_col)

        # Manage Skipped on hoursofsleep
        df_user["num_hours_slept"] = df_user["num_hours_slept"].replace(
            "Skipped", np.nan
        )

        # Write to database
        database.build_impact_user(df_user, self._ref_maps)
        log.write.info(
            f"Finished building tbl_impact_user for visit: {self._test_type}"
        )

    def _word_data(self):
        """Title."""
        #
        log.write.info(
            f"Building tbl_impact_word for visit: {self._test_type}"
        )

        # Extract identifier and wordItem columns
        col_word = [x for x in self._df_clean.columns if "word" in x]
        col_word = [
            "subj_id",
            "base_id",
            "post_id",
            "rtp_id",
            "tbi_num",
            "testType",
            "test_id",
        ] + col_word
        idx_type = self._df_clean.index[
            self._df_clean["testType"] == self._test_type
        ].tolist()
        df_word = self._df_clean.loc[idx_type, col_word].copy()

        #
        rename_cols = {
            "wordMemoryHits",
            "wordMemoryCD",
            "wordMemoryLP",
            "wordMemoryHitsDelay",
            "wordMemoryCDDelay",
            "wordMemoryDMCorrect",
            "wordMemoryTotalPercentCorr",
        }
