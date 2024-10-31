"""Title.

TODO

"""

import os
import pandas as pd
import openpyxl  # noqa: F401
import numpy as np
from adr_dwi import database
from adr_dwi import helper

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


def setup_db():
    """Title."""
    log.write.info("Starting setup_db")
    raw_demo = os.path.join(
        os.environ["HOME"],
        "Projects",
        "data",
        "participant_list_for_nate.xlsx",
    )
    if not os.path.exists(raw_demo):
        raise FileNotFoundError(raw_demo)
    df = pd.read_excel(raw_demo)

    #
    df = df[df["SubjectID_baseline"].notna()]
    df.columns = df.columns.str.replace("#", "")
    df.columns = df.columns.str.replace("-", "_")
    df.columns = df.columns.str.replace(" ", "_")

    #
    df["subj_id"] = df["SubjectID_baseline"].str[3:7].astype(int)
    df["subj_name"] = df["SubjectID_baseline"].str[3:7]
    df["subj_sky_base"] = df["Baseline_SKY_"]
    df["subj_sky_fu1"] = df["Follow_up_1_SKY_"]
    df["subj_sky_fu2"] = df["Follow_up_2_SKY_"]
    df["subj_sky_twin"] = df["Twin_SKY_"]

    #
    col_list = [
        "subj_id",
        "subj_name",
        "subj_sky_base",
        "subj_sky_fu1",
        "subj_sky_fu2",
        "subj_sky_twin",
    ]
    df_subj = df[col_list].copy()
    df_subj = df_subj.replace(np.nan, None)

    #
    db_con = database.DbConnect()
    # database.update_ref_subj(df_subj, col_list, db_con)

    #
    df["sex"] = df["Gender"].str[:1]
    df["age_base"] = df["Age_at_Baseline"].astype(int)
    col_list = ["subj_id", "sex", "age_base"]
    df_demo = df[col_list].copy()
    df_demo = df_demo.replace(np.nan, None)

    # database.update_tbl_demo(df_demo, col_list, db_con)

    #
    df["base"] = df["Date_of_Baseline"]
    df["fu1"] = df["Date_of_Follow_up_1"]
    df["fu2"] = df["Date_of_Follow_up_2"]
    df_scan = df[["subj_id", "base", "fu1", "fu2"]].copy()
    df_long = pd.melt(
        df_scan,
        id_vars=["subj_id"],
        value_vars=["base", "fu1", "fu2"],
        var_name="scan_name",
        value_name="scan_date",
        ignore_index=False,
    )
    # df_long["scan_date"] = df_long["scan_date"].dt.strftime("%Y-%m-%d")
    df_long["scan_date"] = df_long["scan_date"].astype(str)
    df_long["scan_date"] = df_long["scan_date"].str[:10]
    df_long.info()
    log.write.info(f"df_long:\n{df_long.tail()}")
    database.update_scan_dates(
        df_long, ["subj_id", "scan_name", "scan_date"], db_con
    )
