"""Methods for coordinating various workflows.

setup_db: Insert data into db_adr from local XLSX files.
clean_rawdata: BIDSify ADR rawdata data.

TODO check data workflow

"""

import os
import glob
import openpyxl  # noqa: F401
from adr_dwi import database
from adr_dwi import build_survey
from adr_dwi import helper
from adr_dwi import process

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


def setup_db():
    """Build db_adr from shared data."""
    log.write.info("Initializing SetupDb")

    # Establish connection
    db_con = database.DbConnect()
    ref_maps = database.RefMaps(db_con)

    # Setup participant demographics
    build_demo = build_survey.BuildPartDemo(db_con, ref_maps)
    build_demo.load_part_demo()
    build_demo.ref_subj()
    build_demo.tbl_demo()
    build_demo.scan_date()

    # Setup impact from primary source
    build_imp_prim = build_survey.BuildImpactPrimary(db_con, ref_maps)
    build_imp_prim.load_data()
    build_imp_prim.make_impact_tables()

    # Setup impact from secondary source
    build_imp_sec = build_survey.BuildImpactSecondary(db_con, ref_maps)
    build_imp_sec.load_data()
    build_imp_sec.make_impact_tables()
    db_con.close_con()


def clean_rawdata(data_dir: PT):
    """Coordinate cleaning of rawdata files.

    Finalize BIDSification process on data exported
    from attic/barbey/shared/ADR. Assumes extra DWI
    and SWI files have been removed.

    Args:
        data_dir: Location of BIDS organized directory.

    Raises:
        FileNotFoundError: Failed to identify subject or session dirs.

    """
    log.write.info("Started cleaning rawdata")
    raw_dir = os.path.join(data_dir, "rawdata")

    # Identify subjects and sessions
    subj_list = sorted(glob.glob(f"{raw_dir}/sub-*"))
    if not subj_list:
        raise FileNotFoundError()
    subj_list = [os.path.basename(x) for x in subj_list]
    subj_sess = {}
    for subj in subj_list:
        sess_list = sorted(glob.glob(f"{raw_dir}/{subj}/ses-*"))
        if not sess_list:
            raise FileNotFoundError()
        subj_sess[subj] = [os.path.basename(x) for x in sess_list]

    # Clean rawdata
    bids_org = process.BidsOrg()
    bids_org.data_dir = data_dir
    for subj, sess_list in subj_sess.items():
        bids_org.subj = subj
        for sess in sess_list:
            log.write.info(f"BIDSifying {subj}, {sess}")
            bids_org.sess = sess
            bids_org.fix_anat_names()
            bids_org.fix_dwi_names()
            bids_org.fix_fmap_names()
            bids_org.fix_fmap_intended()
            bids_org.fix_fmap_vols()


def preproc_dwi(subj: str, sess: str, data_dir: PT, work_dir: PT, log_dir: PT):
    """Title."""
    log.write.info(f"Starting preproc_dwi: {subj}, {sess}")

    # subj_data = os.path.join(data_dir, "rawdata", subj, sess)
    # subj_work = os.path.join(work_dir, "dwi_preproc", subj, sess, "dwi")
    # if not os.path.exists(subj_work):
    #     os.makedirs(subj_work)

    #
    out_name = f"{subj}_{sess}_dir-AP_desc-eddy_dwi.nii.gz"
    out_dir = os.path.join(
        data_dir, "derivatives", "dwi_preproc", subj, sess, "dwi"
    )
    if not os.path.exists(out_dir):
        os.makedirs(out_dir)

    out_path = os.path.join(out_dir, out_name)
    if os.path.exists(out_path):
        return out_path

    #
    #
    dwi_pp = process.DwiPreproc(subj, sess, work_dir, data_dir, log_dir)
    dwi_dict, fmap_dict = dwi_pp.setup()

    # Get AP, PA files
    b0_dict = {}
    for fmap_type, fmap_path in fmap_dict.items():
        if "fmap" not in fmap_type:
            continue
        dir_val = fmap_type.split("_")[1]
        b0_dict[f"b0_{dir_val}"] = dwi_pp.extract_b0(
            fmap_path, f"tmp_{dir_val}_b0"
        )

    #
    ap_pa_b0 = dwi_pp.combine_b0(
        b0_dict["b0_AP"], b0_dict["b0_PA"], "tmp_AP_PA_b0"
    )
    acq_param = dwi_pp.acq_param(fmap_dict["json_AP"])
    dwi_topup, dwi_unwarp = dwi_pp.run_topup(ap_pa_b0, acq_param)

    #
    dwi_mask = dwi_pp.brain_mask(dwi_unwarp)
    dwi_idx = dwi_pp.write_index(dwi_dict["dwi"])
    dwi_eddy = dwi_pp.run_eddy(
        dwi_dict["dwi"],
        dwi_dict["bvec"],
        dwi_dict["bval"],
        dwi_topup,
        dwi_mask,
        dwi_idx,
        acq_param,
        out_name,
    )
