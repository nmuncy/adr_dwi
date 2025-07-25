"""Clean shared ImPACT data and coordinate inserting into database.

Notes:
    This module is written for an internal data share which assumes very
    specific formats for each shared dataframe.

BuildPartDemo: Clean and build participant demographics table.
BuildImpactPrimary: Clean and build impact tables from primary share.
BuildImpactSecondary: Clean and build impact tables from secondary share.

"""

import os
from typing import Type
import pandas as pd
import openpyxl  # noqa: F401
import numpy as np
from adr_dwi import database
from adr_dwi import helper

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


class BuildPartDemo:
    """Build participant demographics table from shared data.

    Clean data from data_dir/raw_particpant_list.xlsx and then
    build the following db_adr tables:
        - ref_subj
        - tbl_demo
        - tbl_scan_dates

    Args:
        data_dir: Location of data directory, containing raw shared files.
        db_con: Database connection object, database.DbConnect instance.
        ref_maps: SQL reference mapping object, database.RefMaps instance.

    Notes:
        - Uses data that has been lightly cleaned manually.
        - Execute method load_part_demo() first (see example).

    Example:
        build_demo = build_survey.BuildPartDemo(*args)
        build_demo.load_part_demo()
        build_demo.ref_subj()
        build_demo.tbl_demo()
        build_demo.scan_date()

    """

    def __init__(
        self,
        data_dir: PT,
        db_con: Type[database.DbConnect],
        ref_maps: Type[database.RefMaps],
    ):
        """Initialize BuildPartDemo."""
        log.write.info("Initializing BuildPartDemo")
        self._db_con = db_con
        self._ref_maps = ref_maps
        self._data_dir = data_dir

    def load_part_demo(self):
        """Load raw participant demographic info.

        Raises:
            FileNotFoundError: Missing raw_participant_list.xlsx in data_dir.

        """
        log.write.info("Loading part demo data")
        raw_demo = os.path.join(self._data_dir, "raw_participant_list.xlsx")
        if not os.path.exists(raw_demo):
            raise FileNotFoundError(raw_demo)
        self._df_part = pd.read_excel(raw_demo)
        self._clean_part()

    def _clean_part(self):
        """Remove subjects without baseline IDs, manage column names."""
        log.write.info("Cleaning participant data")
        self._df_part = self._df_part[
            self._df_part["SubjectID_baseline"].notna()
        ]
        self._df_part.columns = self._df_part.columns.str.replace("#", "")
        self._df_part.columns = self._df_part.columns.str.replace("-", "_")
        self._df_part.columns = self._df_part.columns.str.replace(" ", "_")

    def ref_subj(self):
        """Build db_adr.ref_subj by aligning subject IDs across visits."""
        log.write.info("Making ref_subj")

        # Manage ID columns
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

        # Extract only subject ID columns
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

        # Prep for database insert - proper column names, format,
        # and foreign keys.
        col_new = {
            x: x.split("_")[-1]
            for x in col_sub
            if x not in ["subj_id", "subj_name"]
        }
        df_subj = df_subj.rename(columns=col_new)
        col_list = [y for x, y in col_new.items()]
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

        # Send data to db_adr.ref_subj
        database.build_table(
            "ref_subj",
            df_long,
            ["subj_id", "subj_name", "sky_type", "sky_name"],
            self._db_con,
        )

    def tbl_demo(self):
        """Build db_adr.tbl_demo with participant demographics."""
        log.write.info("Making tbl_demo")

        # Find relevant columns
        self._df_part["sex"] = self._df_part["Gender"].str[:1]
        self._df_part["age_base"] = self._df_part["Age_at_Baseline"].astype(
            int
        )

        # Subset and insert
        col_list = ["subj_id", "sex", "age_base"]
        df_demo = self._df_part[col_list].copy()
        df_demo = df_demo.replace(np.nan, None)
        database.build_table("tbl_demo", df_demo, col_list, self._db_con)

    def scan_date(self):
        """Build db_adr.tbl_scan_dates with participant scan dates."""
        log.write.info("Making tbl_scan_date")

        # Identify relevant columns
        self._df_part["base"] = self._df_part["Date_of_Baseline"]
        self._df_part["fu1"] = self._df_part["Date_of_Follow_up_1"]
        self._df_part["fu2"] = self._df_part["Date_of_Follow_up_2"]

        # Format to db_adr
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

        # Add foreign keys and manage date values, insert into table
        df_long["scan_id"] = df_long.apply(
            lambda x: self._ref_maps.get_id("test", x, "scan_name"), axis=1
        )
        df_long["scan_date"] = df_long["scan_date"].astype(str)
        df_long["scan_date"] = df_long["scan_date"].str[:10]

        database.build_table(
            "tbl_scan_dates",
            df_long,
            ["subj_id", "scan_id", "scan_date"],
            self._db_con,
        )


class BuildImpactPrimary:
    """Build impact tables from primary shared data.

    Clean data from data_dir/raw_impact_a.xlsx and then build
    the following db_adr tables:
        - tbl_impact_dates
        - tbl_impact_user
        - tbl_impact_word
        - tbl_impact_design
        - tbl_impact_xo
        - tbl_impact_color
        - tbl_impact_three

    Args:
        data_dir: Location of data directory, containing raw shared files.
        db_con: Database connection object, database.DbConnect instance.
        ref_maps: SQL reference mapping object, database.RefMaps instance.

    Notes:
        - Uses data that has been lightly cleaned manually.
        - Execute method load_data() first (see example).

    Example:
        build_imp_prim = build_survey.BuildImpactPrimary(*args)
        build_imp_prim.load_data()
        build_imp_prim.make_impact_tables()

    """

    def __init__(
        self,
        data_dir: PT,
        db_con: Type[database.DbConnect],
        ref_maps: Type[database.RefMaps],
    ):
        """Initialize BuildImpactPrimary."""
        log.write.info("Initializing BuildImpactPrimary")
        self._db_con = db_con
        self._ref_maps = ref_maps
        self._data_dir = data_dir

    def load_data(self):
        """Load raw impact data, uses sheet titled 'v1_clean'.

        Raises:
            FileNotFoundError: Missing raw_impact_a.xlsx in data_dir.

        """
        log.write.info("Loading primary impact data")
        raw_impact = os.path.join(self._data_dir, "raw_impact_a.xlsx")
        if not os.path.exists(raw_impact):
            raise FileNotFoundError(raw_impact)
        self._df_clean = pd.read_excel(raw_impact, "v1_clean")
        self._clean_impact()

    def _clean_impact(self):
        """Clean impact data."""
        log.write.info("Cleaning primary impact data")

        # Match impact name with db_adr key values
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

        # Forward fill subj_ids and manage type, update column names
        self._df_clean["subj_id"] = self._df_clean["subj_id"].ffill(axis=0)
        self._df_clean["subj_id"] = self._df_clean["subj_id"].astype("Int64")
        self._df_clean = self._df_clean.rename(columns={"tbi_num": "num_tbi"})

    def make_impact_tables(self):
        """Entrypoint method for sending impact data to db_adr."""
        # Coordinate private methods.
        for self._test_type in self._test_type_list:
            self._impact_date()
            self._user_data()
            self._word_data()
            self._design_data()
            self._xo_data()
            self._color_data()
            self._three_data()

    def _impact_date(self):
        """Send data to db_adr.tbl_impact_dates."""
        # Subset with relevant data
        col_date = [
            "subj_id",
            "test_id",
            "testType",
            "testDate",
            "num_tbi",
        ]
        idx_type = self._df_clean.index[
            self._df_clean["testType"] == self._test_type
        ].tolist()
        df_dates = self._df_clean.loc[idx_type, col_date].copy()

        # Manage datetime type and column, insert data
        df_dates["testDate"] = df_dates["testDate"].dt.strftime("%Y-%m-%d")
        df_dates = df_dates.rename(columns={"testDate": "impact_date"})
        database.build_table(
            "tbl_impact_dates",
            df_dates,
            ["subj_id", "test_id", "num_tbi", "impact_date"],
            self._db_con,
        )

    def _user_data(self):
        """Send data to db_adr.tbl_impact_user."""
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
            "num_tbi",
            "testType",
            "test_id",
            "testDate",
        ] + col_user
        idx_type = self._df_clean.index[
            self._df_clean["testType"] == self._test_type
        ].tolist()
        df_user = self._df_clean.loc[idx_type, col_user].copy()

        # Make symptom delayed int
        delay_col = [x for x in df_user.columns if "Delayed" in x]
        df_user[delay_col] = df_user[delay_col].astype("Int64")

        # Manage Skipped on hoursofsleep
        df_user["userHoursOfSleepLastNight"] = df_user[
            "userHoursOfSleepLastNight"
        ].replace("Skipped", np.nan)

        # Write to database
        database.build_impact_user(df_user, self._ref_maps)
        log.write.info(
            f"Finished building tbl_impact_user for visit: {self._test_type}"
        )

    def _word_data(self):
        """Send data to db_adr.tbl_impact_word."""
        log.write.info(
            f"Building tbl_impact_word for visit: {self._test_type}"
        )

        # Extract identifier and wordItem columns
        col_sub = [x for x in self._df_clean.columns if "word" in x]
        col_sub = [
            "subj_id",
            "base_id",
            "post_id",
            "rtp_id",
            "num_tbi",
            "testType",
            "test_id",
        ] + col_sub
        idx_type = self._df_clean.index[
            self._df_clean["testType"] == self._test_type
        ].tolist()
        df = self._df_clean.loc[idx_type, col_sub].copy()

        # Identify relevant columns
        col_list = [
            "subj_id",
            "test_id",
            "num_tbi",
            "wordMemoryHits",
            "wordMemoryHitsDelay",
            "wordMemoryCD",
            "wordMemoryCDDelay",
            "wordMemoryLP",
            "wordMemoryDMCorrect",
            "wordMemoryTotalPercentCorrect",
        ]
        database.build_table("tbl_impact_word", df, col_list, self._db_con)

    def _design_data(self):
        """Send data to db_adr.tbl_impact_design."""
        log.write.info(
            f"Building tbl_impact_design for visit: {self._test_type}"
        )

        # Extract identifier and wordItem columns
        col_sub = [x for x in self._df_clean.columns if "design" in x]
        col_sub = [
            "subj_id",
            "base_id",
            "post_id",
            "rtp_id",
            "num_tbi",
            "testType",
            "test_id",
        ] + col_sub
        idx_type = self._df_clean.index[
            self._df_clean["testType"] == self._test_type
        ].tolist()
        df = self._df_clean.loc[idx_type, col_sub].copy()

        # Identify relevant columns
        col_list = [
            "subj_id",
            "test_id",
            "num_tbi",
            "designMemoryHits",
            "designMemoryHitsDelay",
            "designMemoryCD",
            "designMemoryCDDelay",
            "designMemoryLP",
            "designMemoryDMCorrect",
            "designMemoryTotalPercentCorrect",
        ]
        database.build_table("tbl_impact_design", df, col_list, self._db_con)

    def _xo_data(self):
        """Send data to db_adr.tbl_impact_xo."""
        log.write.info(f"Building tbl_impact_xo for visit: {self._test_type}")

        # Extract identifier and xo columns
        col_sub = [x for x in self._df_clean.columns if "XO" in x]
        col_sub = [
            "subj_id",
            "base_id",
            "post_id",
            "rtp_id",
            "num_tbi",
            "testType",
            "test_id",
        ] + col_sub
        idx_type = self._df_clean.index[
            self._df_clean["testType"] == self._test_type
        ].tolist()
        df = self._df_clean.loc[idx_type, col_sub].copy()

        # Identify relevant columns
        col_list = [
            "subj_id",
            "test_id",
            "num_tbi",
            "XOtotalCorrectMemory",
            "XOtotalCorrectInterference",
            "XOaverageCorrect",
            "XOtotalIncorrect",
            "XOaverageIncorrect",
        ]
        database.build_table("tbl_impact_xo", df, col_list, self._db_con)

    def _color_data(self):
        """Send data to db_adr.tbl_impact_color."""
        log.write.info(
            f"Building tbl_impact_color for visit: {self._test_type}"
        )

        # Extract identifier and color columns
        col_sub = [x for x in self._df_clean.columns if "color" in x]
        col_sub = [
            "subj_id",
            "base_id",
            "post_id",
            "rtp_id",
            "num_tbi",
            "testType",
            "test_id",
        ] + col_sub
        idx_type = self._df_clean.index[
            self._df_clean["testType"] == self._test_type
        ].tolist()
        df = self._df_clean.loc[idx_type, col_sub].copy()

        # Identify relevant columns
        col_list = [
            "subj_id",
            "test_id",
            "num_tbi",
            "colorMatchTotalCorrect",
            "colorMatchAverageCorrect",
            "colorMatchTotalCommissions",
            "colorMatchAverageCommissions",
        ]
        database.build_table("tbl_impact_color", df, col_list, self._db_con)

    def _three_data(self):
        """Send data to db_adr.tbl_impact_three."""
        log.write.info(
            f"Building tbl_impact_three for visit: {self._test_type}"
        )

        # Extract identifier and three columns
        col_sub = [x for x in self._df_clean.columns if "three" in x]
        col_sub = [
            "subj_id",
            "base_id",
            "post_id",
            "rtp_id",
            "num_tbi",
            "testType",
            "test_id",
        ] + col_sub
        idx_type = self._df_clean.index[
            self._df_clean["testType"] == self._test_type
        ].tolist()
        df = self._df_clean.loc[idx_type, col_sub].copy()

        # Manage precision
        df["threeLettersPercentageLettersCorrect"] = df[
            "threeLettersPercentageLettersCorrect"
        ].round(2)

        # Identify relevant columns
        col_list = [
            "subj_id",
            "test_id",
            "num_tbi",
            "threeLettersTotalSequenceCorrect",
            "threeLettersTotalLettersCorrect",
            "threeLettersPercentageLettersCorrect",
            "threeLettersAverageTimeFirstClick",
            "threeLettersAverageCounted",
            "threeLettersAverageCountedCorrectly",
        ]
        database.build_table("tbl_impact_three", df, col_list, self._db_con)


class BuildImpactSecondary:
    """Build impact tables from secondary shared data.

    Clean data from data_dir/raw_impact_b.xlsx and then build
    the following db_adr tables:
        - tbl_impact_dates
        - tbl_impact_user
        - tbl_impact_design
        - tbl_impact_xo
        - tbl_impact_color
        - tbl_impact_three

    Args:
        data_dir: Location of data directory, containing raw shared files.
        db_con: Database connection object, database.DbConnect instance.
        ref_maps: SQL reference mapping object, database.RefMaps instance.

    Notes:
        - Uses data that has been lightly cleaned manually.
        - Execute method load_data() first (see example).

    Example:
        build_imp_sec = build_survey.BuildImpactSecondary(*args)
        build_imp_sec.load_data()
        build_imp_sec.make_impact_tables()

    """

    def __init__(
        self,
        data_dir: PT,
        db_con: Type[database.DbConnect],
        ref_maps: Type[database.RefMaps],
    ):
        """Initialize BuildImpactSecondary."""
        log.write.info("Initializing BuildImpactSecondary")
        self._db_con = db_con
        self._ref_maps = ref_maps
        self._data_dir = data_dir

    def load_data(self):
        """Load raw impact data.

        Raises:
            FileNotFoundError: Missing raw_impact_b.xlsx in data_dir.

        """
        log.write.info("Loading secondary impact data")
        raw_impact = os.path.join(self._data_dir, "raw_impact_b.xlsx")
        if not os.path.exists(raw_impact):
            raise FileNotFoundError(raw_impact)
        self._df_clean = pd.read_excel(raw_impact)

    def make_impact_tables(self):
        """Entrypoint method for sending impact data to db_adr."""
        test_type_list = self._df_clean["test_id"].unique()
        for self._test_id in test_type_list:
            self._impact_date()
            self._user_data()
            self._other_data()

    def _impact_date(self):
        """Send data to db_adr.tbl_impact_dates."""
        log.write.info(f"Building tbl_impact_dates for visit: {self._test_id}")

        # Identify relevant columns
        col_date = [
            "subj_id",
            "test_id",
            "num_tbi",
            "testDate",
        ]
        idx_type = self._df_clean.index[
            self._df_clean["test_id"] == self._test_id
        ].tolist()
        df_dates = self._df_clean.loc[idx_type, col_date].copy()

        # Manage col types and names
        df_dates["testDate"] = df_dates["testDate"].dt.strftime("%Y-%m-%d")
        df_dates = df_dates.rename(columns={"testDate": "impact_date"})
        df_dates = df_dates.replace(np.nan, None)

        # Send data to database
        database.build_table(
            "tbl_impact_dates",
            df_dates,
            ["subj_id", "test_id", "num_tbi", "impact_date"],
            self._db_con,
        )

    def _user_data(self):
        """Send data to db_adr.tbl_impact_user."""
        log.write.info(f"Building tbl_impact_user for visit: {self._test_id}")

        # Extract identifier and userItem columns
        col_user = [x for x in self._df_clean.columns if "user" in x]
        col_user = [
            "subj_id",
            "test_id",
            "num_tbi",
        ] + col_user
        idx_type = self._df_clean.index[
            self._df_clean["test_id"] == self._test_id
        ].tolist()
        df_user = self._df_clean.loc[idx_type, col_user].copy()
        df_user = df_user.replace(np.nan, None)

        # Write to database
        database.build_table(
            "tbl_impact_user", df_user, col_user, self._db_con
        )

    def _other_data(self):
        """Send data to db_adr tables.

        Send data to tbl_impact_design, tbl_impact_xo, tbl_impact_color,
        and tbl_impact_three.

        """
        # Determine id and table columns
        id_cols = ["subj_id", "test_id", "num_tbi"]
        map_cols = {
            "design": [x for x in self._df_clean.columns if "design" in x],
            "xo": [x for x in self._df_clean.columns if "XO" in x],
            "color": [x for x in self._df_clean.columns if "color" in x],
            "three": [x for x in self._df_clean.columns if "three" in x],
        }

        # Send data to each planned table
        for tbl, sub_cols in map_cols.items():
            log.write.info(
                f"Building tbl_impact_{tbl} for visit: {self._test_id}"
            )
            col_list = id_cols + sub_cols
            idx_type = self._df_clean.index[
                self._df_clean["test_id"] == self._test_id
            ].tolist()
            df = self._df_clean.loc[idx_type, col_list].copy()
            df = df.replace(np.nan, None)

            # Write to database
            database.build_table(
                f"tbl_impact_{tbl}", df, col_list, self._db_con
            )
