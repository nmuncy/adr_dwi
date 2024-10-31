"""Title.

TODO

"""

import os
import pandas as pd
import numpy as np
from adr_dwi import database
from adr_dwi import helper

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


def setup_db():
    """Title."""
    log.write.info("Starting setup_db")
    raw_demo = os.path.join(
        os.environ["HOME"], "Desktop", "participant_list_for_nate.xlsx"
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
    df_insert = df[col_list].copy()
    df_insert = df_insert.replace(np.NaN, None)
    log.write.info(f"df_insert:\n{df_insert}")

    #
    db_con = database.DbConnect()
    database.update_ref_subj(df_insert, col_list, db_con)
