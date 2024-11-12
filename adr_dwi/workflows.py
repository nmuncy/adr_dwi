"""Title.

TODO

"""

import os
import pandas as pd
import openpyxl  # noqa: F401
import numpy as np
from adr_dwi import database
from adr_dwi import build_survey
from adr_dwi import helper

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


def setup_db():
    """Title."""
    log.write.info("Initializing SetupDb")

    #
    db_con = database.DbConnect()
    ref_maps = database.RefMaps(db_con)

    #
    build_demo = build_survey.BuildPartDemo(db_con, ref_maps)
    build_demo.load_part_demo()
    build_demo.ref_subj()
    build_demo.tbl_demo()
    build_demo.scan_date()

    #
    build_imp_prim = build_survey.BuildImpactPrimary(db_con, ref_maps)
    build_imp_prim.load_data()
    build_imp_prim.make_impact_tables()

    #
    build_imp_sec = build_survey.BuildImpactSecondary(db_con, ref_maps)
    build_imp_sec.load_data()
    build_imp_sec.make_impact_tables()
    db_con.close_con()


def build_rawdata(data_dir: PT):
    """Title."""
    log.write.info("Started building rawdata")
    raw_dir = os.path.join(data_dir, "rawdata")
    if not os.path.exists(raw_dir):
        os.makedirs(raw_dir)

    # TODO get subject list
    db_con = database.DbConnect()
    sql_cmd = (
        "select distinct b.subj_name from tbl_scan_dates a "
        + "join ref_subj b on a.subj_id=b.subj_id where a.scan_id=1;"
    )
    subj_list = [f"sub-{x[0]}" for x in db_con.fetch_rows(sql_cmd)]
    log.write.debug(f"subj_list: {subj_list}")
    db_con.close_con()

    # TODO pull subject data from attic
    # TODO bidsify rawdata
