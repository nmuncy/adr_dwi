"""Methods for coordinating various workflows.

When the same workflow is conducted for multiple subjects, a wrapper
method is provided to allow for scheduling all subjects as an array.

clean_rawdata: BIDSify ADR rawdata data.
insert_impactg: Send impact data to db_adr.
insert_pyafq: Send pyAFQ node metrics to db_adr.
preproc_dwi: Preprocess DWI data with FSL.
run_pyafq: Run pyAFQ workflow on preprocessed DWI data.
setup_pyafq: Setup for running pyAFQ on preprocessed DWI data.
setup_db: Insert data into db_adr from local XLSX files.
wrap_preproc_dwi: Trigger preproc_dwi as part of scheduled array.
wrap_setup_afq: Trigger setup_pyafq as part of scheduled array.

"""

import os
import glob
import shutil
import json
import toml
import platform
import pandas as pd
import openpyxl  # noqa: F401
from adr_dwi import database
from adr_dwi import build_survey
from adr_dwi import helper
from adr_dwi import process
from adr_dwi import submit

import importlib.resources as pkg_resources
from adr_dwi import bin as adr_bin

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


def insert_impact():
    """Clean then add impact and demographics to db_adr from shared data.

    Raises:
        EnvironmentError: Attempt to execute function on
            system other than HCC.

    """
    log.write.info("Initializing SetupDb")

    # Verify env
    if "swan" not in platform.uname().node:
        raise EnvironmentError(
            "workflows.setub_db is written for execution on HCC."
        )

    # Establish connection
    # TODO resolve updates to RefMaps that invlolve pulling data,
    #       if rebuilding db, exec setup_db twice.
    db_con = database.DbConnect()
    ref_maps = database.RefMaps(db_con)

    # Setup participant demographics
    data_dir = "/mnt/nrdstor/muncylab/nmuncy2/ADR/data_impact"
    build_demo = build_survey.BuildPartDemo(data_dir, db_con, ref_maps)
    build_demo.load_part_demo()
    build_demo.ref_subj()
    build_demo.tbl_demo()
    build_demo.scan_date()

    # Setup impact from primary source
    build_imp_prim = build_survey.BuildImpactPrimary(
        data_dir, db_con, ref_maps
    )
    build_imp_prim.load_data()
    build_imp_prim.make_impact_tables()

    # Setup impact from secondary source
    build_imp_sec = build_survey.BuildImpactSecondary(
        data_dir, db_con, ref_maps
    )
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


def preproc_dwi(
    subj: str,
    sess: str,
    data_dir: PT,
    work_dir: PT,
    log_dir: PT,
    run: int = None,
) -> PT:
    """Conduct preprocessing of DWI via FSL.

    The preprocessing steps are:
        1. Copy required files from data to work dir.
        2. Extract b0 volumes from AP and PA fmaps.
        3. Combine AP+PA b0 files.
        4. Write acquisition parameters for AP+PA file.
        5. Run topup for distortion correction calculations.
        6. Make a brain mask.
        7. Build an index file.
        8. Preprocess DWI data via eddy.
        9. Send eddy output to data dir, clean work.

    Args:
        subj: BIDS subject ID.
        sess: BIDS session ID.
        work_dir: Location for intermediates.
        data_dir: Location of BIDS organized directory.
        log_dir: Location for capturing STDOUT/ERR.
        run: Optional, run ID for scan-rescan of same session.

    Returns:
        Location of eddy output file in data dir.

    Raises:
        FileNotFoundError: Missing final eddy file.

    Returns:
        Location of preprocessed DWI file.

    """
    log.write.info(f"Starting preproc_dwi: {subj}, {sess}")

    # Set output name and dir
    out_name = (
        f"{subj}_{sess}_dir-AP_desc-eddy_dwi.nii.gz"
        if not run
        else f"{subj}_{sess}_dir-AP_run-{run}_desc-eddy_dwi.nii.gz"
    )
    out_dir = os.path.join(
        data_dir, "derivatives", "dwi_preproc", subj, sess, "dwi"
    )
    if not os.path.exists(out_dir):
        os.makedirs(out_dir)

    # Avoid repeating work
    out_path = os.path.join(out_dir, out_name)
    if os.path.exists(out_path):
        return out_path

    # Set out names accounting for run use
    comb_out = f"tmp_AP_PA_r{run}_b0" if run else "tmp_AP_PA_b0"
    top_out = f"tmp_topup_r{run}" if run else "tmp_topup"
    param_out = f"tmp_r{run}_acq_param.txt" if run else "tmp_acq_param.txt"
    mask_out = f"tmp_r{run}_brain.nii.gz" if run else "tmp_brain.nii.gz"
    idx_out = f"tmp_r{run}_index.txt" if run else "tmp_index.txt"

    # Setup for preprocessing
    dwi_pp = process.DwiPreproc(subj, sess, work_dir, data_dir)
    if run:
        dwi_pp.run = run
    dwi_dict, fmap_dict = dwi_pp.setup()

    # Get AP, PA files
    b0_dict = {}
    for fmap_type, fmap_path in fmap_dict.items():
        if "fmap" not in fmap_type:
            continue
        dir_val = fmap_type.split("_")[1]
        tmp_out = f"tmp_{dir_val}_r{run}_b0" if run else f"tmp_{dir_val}_b0"
        b0_dict[f"b0_{dir_val}"] = dwi_pp.extract_b0(fmap_path, tmp_out)

    # Preprocess for topup
    ap_pa_b0 = dwi_pp.combine_b0(
        b0_dict["b0_AP"], b0_dict["b0_PA"], out_name=comb_out
    )
    acq_param = dwi_pp.acq_param(fmap_dict["json_AP"], out_name=param_out)
    dwi_topup, dwi_unwarp = dwi_pp.run_topup(
        ap_pa_b0,
        acq_param,
        f"topup_{subj[4:]}_{sess[4:]}",
        log_dir,
        out_name=top_out,
    )

    # Preprocess with eddy
    dwi_mask = dwi_pp.brain_mask(dwi_unwarp, out_name=mask_out)
    dwi_idx = dwi_pp.write_index(dwi_dict["dwi"], out_name=idx_out)
    dwi_eddy = dwi_pp.run_eddy(
        dwi_dict["dwi"],
        dwi_dict["bvec"],
        dwi_dict["bval"],
        dwi_dict["json"],
        dwi_topup,
        dwi_mask,
        dwi_idx,
        acq_param,
        out_name,
        f"eddy_{subj[4:]}_{sess[4:]}",
        log_dir,
    )

    # Save eddy output, also send bval/json
    eddy_dir = os.path.dirname(dwi_eddy)
    _, _ = submit.simp_subproc(f"cp {eddy_dir}/*eddy* {out_dir}")
    for file_name, file_path in dwi_dict.items():
        if file_name not in ["bval", "json"]:
            continue
        _, _ = submit.simp_subproc(f"cp {file_path} {out_dir}")

    # Clean session dir
    if not os.path.exists(out_path):
        raise FileNotFoundError(out_path)
    if run:
        return
    shutil.rmtree(os.path.dirname(os.path.dirname(dwi_eddy)))
    log.write.info(f"Finished preproc_dwi: {subj}, {sess}")
    return out_path


def wrap_preproc_dwi(
    subj_sess: list,
    data_dir: PT,
    work_dir: PT,
    log_dir: PT,
):
    """Submit preproc_dwi for array task ID.

    Use array task ID to identify subject and session in subj_sess.

    Args:
        subj_sess: Tuples of BIDS subject, session IDs.
        data_dir: BIDS data location.
        work_dir: Location for intermediates.
        log_dir: Location for writing stdout/err.

    Raises:
        EnvironmentError: OS global variable 'SLURM_ARRAY_TASK_ID' not found.

    """
    try:
        arr_id = os.environ["SLURM_ARRAY_TASK_ID"]
    except KeyError:
        log.write.error(
            f"{wrap_preproc_dwi.__name__} intended for "
            + "execution by SLURM array"
        )
        raise EnvironmentError()

    # Identify iteration subject and session list
    subj, sess = subj_sess[int(arr_id)]
    log.write.info(f"Starting preproc_dwi for: {subj}, {sess}")
    _ = preproc_dwi(subj, sess, data_dir, work_dir, log_dir)


def setup_pyafq(
    subj: str,
    sess: str,
    data_dir: PT,
    work_dir: PT,
    run: str = None,
) -> PT:
    """Setup for running pyAFQ.

    Copy relevant files from data_dir to work_dir and then generate a
    brain mask via FSL's bet.

    Requires:
        FSL to be executable in system OS.

    Args:
        subj: BIDS subject ID.
        sess: BIDS session ID.
        data_dir: Location of BIDS organized directory.
        work_dir: Location for intermediates.
        run: Optional, BIDS run ID for scan-rescan of same session.

    Raises:
        EnvironmentError: FSL not executable in system OS.
        FileNotFoundError: Missing expected preprocessed DWI,
            or final output file.

    Returns:
        Location of brain mask.

    """
    # Validate environment
    if not shutil.which("bet"):
        raise EnvironmentError("Missing FSL in environment")

    # Setup, avoid repeating work
    subj_work = os.path.join(work_dir, "dwi_afq", subj, sess, "dwi")
    if not os.path.exists(subj_work):
        os.makedirs(subj_work)
    out_mask = os.path.join(
        subj_work, f"{subj}_{sess}_dir-AP_desc-brain_mask.nii.gz"
    )
    if run:
        out_mask = out_mask.replace("_desc-brain", f"_{run}_desc-brain")
    if os.path.exists(out_mask):
        return out_mask

    # Identify files to pull
    log.write.info(f"Running setup_afq for: {subj}, {sess}")
    subj_data = os.path.join(
        data_dir, "derivatives", "dwi_preproc", subj, sess, "dwi"
    )
    preproc_dict = {
        "dwi": f"{subj}_{sess}_dir-AP_desc-eddy_dwi.nii.gz",
        "bvec": f"{subj}_{sess}_dir-AP_desc-eddy_dwi.eddy_rotated_bvecs",
        "bval": f"{subj}_{sess}_dir-AP_dwi.bval",
        "json": f"{subj}_{sess}_dir-AP_dwi.json",
    }
    if run:
        for key, value in preproc_dict.items():
            preproc_dict[key] = value.replace("_dir-AP", f"_dir-AP_{run}")

    # Pull files
    for file_type, file_name in preproc_dict.items():
        file_path = os.path.join(subj_data, file_name)
        if not os.path.exists(file_path):
            raise FileNotFoundError(file_path)

        # Rename bvec file
        if file_type == "bvec":
            file_pref = os.path.splitext(file_name)[0]
            out_path = os.path.join(subj_work, f"{file_pref}.bvec")
        else:
            out_path = os.path.join(subj_work, file_name)

        _, _ = submit.simp_subproc(f"cp {file_path} {out_path}")

    # Make a brain mask from preprocessed data
    fsl_cmd = [
        f"cd {subj_work};",
        f"bet {preproc_dict["dwi"]}",
        f"{out_mask.split('_mask')[0]}",
        "-f 0.2",
        "-g 0",
        "-m -n",
    ]
    out, err = submit.simp_subproc(" ".join(fsl_cmd))

    # Validate output
    if not os.path.exists(out_mask):
        log.write.debug(f"Bet STDOUT: {out}")
        log.write.debug(f"Bet STDERR: {err}")
        raise FileNotFoundError(out_mask)
    return out_mask


def wrap_setup_pyafq(
    subj_sess: list,
    data_dir: PT,
    work_dir: PT,
    run_list: list = None,
):
    """Submit setup_pyafq for array task ID.

    Write a dataset_description.json and config.toml (adr_dwi.bin.config.toml)
    to meet pyAFQ requirements, then use array task ID to identify subject and
    session in subj_sess.

    Args:
        subj_sess: Tuples of BIDS subject, session IDs.
        data_dir: BIDS data location.
        work_dir: Location for intermediates.
        run_list: Optional, list of run IDs for scan-rescan of same session.

    Raises:
        EnvironmentError: OS global variable 'SLURM_ARRAY_TASK_ID' not found.

    """
    try:
        arr_id = os.environ["SLURM_ARRAY_TASK_ID"]
    except KeyError:
        log.write.error(
            f"{wrap_preproc_dwi.__name__} intended for "
            + "execution by SLURM array"
        )
        raise EnvironmentError()

    # Make dataset_description
    log.write.info("Writing dataset_description")
    data_desc = {
        "Name": "ADR",
        "BIDSVersion": "1.10.0",
        "GeneratedBy": [{"Name": "FSL"}],
    }
    work_deriv = os.path.join(work_dir, "dwi_afq")
    with open(os.path.join(work_deriv, "dataset_description.json"), "w") as jf:
        json.dump(data_desc, jf)

    # Write config.toml from resources, update bids path
    log.write.info("Writing config.toml")
    config_data = toml.load(
        pkg_resources.files(adr_bin).joinpath("config.toml").open("r")
    )
    config_data["BIDS_PARAMS"]["bids_path"] = work_deriv
    work_config = os.path.join(work_deriv, "config.toml")
    with open(work_config, "w") as cnf_out:
        toml.dump(config_data, cnf_out)

    # Identify iteration subject and session list
    subj, sess = subj_sess[int(arr_id)]
    if run_list:
        for run_id in run_list:
            run = f"run-{run_id}"
            log.write.info(f"Starting setup_afq for: {subj}, {sess}, {run}")
            _ = setup_pyafq(subj, sess, data_dir, work_dir, run=run)
        return

    log.write.info(f"Starting setup_afq for: {subj}, {sess}")
    _ = setup_pyafq(subj, sess, data_dir, work_dir)


def run_pyafq(data_dir: PT, work_dir: PT, log_dir: PT, rerun: bool) -> PT:
    """Run pyAFQ workflow.

    Use preprocessed DWI data to conduct tractography via pyAFQ. Assumes
    the output of preproc_dwi() has been properly organized (setup_pyafq())
    for all subjects. Output will be found in data_dir/derivatives/afq.

    Requires:
        Global variable 'SING_PYAFQ' to hold path to singularity image of AFQ.

    Args:
        data_dir: Location of BIDS organized directory.
        work_dir: Location for intermediates.
        log_dir: Location for writing stdout/err.
        rerun: Keep derivatives separate from regular workflow.

    Raises:
        KeyError: Missing global variable 'SING_PYAFQ'.
        FileNotFoundError: Missing expected output in work_dir or data_dir.

    Returns:
        Location of output tract_profiles.csv.

    """
    # Check env
    try:
        sing_afq = os.environ["SING_PYAFQ"]
    except KeyError as e:
        log.write.error("Missing required variable SING_PYAFQ")
        raise e

    # Avoid repeating work
    log.write.info("Starting pyAFQ")
    data_deriv = (
        os.path.join(data_dir, "derivatives_rerun")
        if rerun
        else os.path.join(data_dir, "derivatives")
    )
    if not os.path.exists(data_deriv):  # Account for rerun output dir
        os.makedirs(data_deriv)
    out_path = os.path.join(data_deriv, "afq", "tract_profiles.csv")
    if os.path.exists(out_path):
        log.write.info("pyAFQ output found")
        return out_path

    # Submit pyAFQ
    work_afq = os.path.join(work_dir, "dwi_afq")
    bash_list = [
        "singularity",
        "run",
        "--cleanenv",
        f"--bind {work_afq}:{work_afq}",
        sing_afq,
        f"{work_afq}/config.toml",
        "--notrack",
    ]
    out, err = submit.sched_subproc(
        " ".join(bash_list),
        "pyAFQ",
        log_dir,
        num_hours=60,
        num_cpus=12,
        mem_gig=32,
    )

    # Check for output in work
    work_dir = os.path.join(work_afq, "derivatives", "afq")
    if not os.path.exists(work_dir):
        log.write.error(f"AFQ STDOUT: {out}")
        log.write.error(f"AFQ STDERR: {err}")
        raise FileNotFoundError(work_dir)

    # Copy and clean work
    _, _ = submit.simp_subproc(f"cp -r {work_dir} {data_deriv}")
    if not os.path.exists(out_path):
        raise FileNotFoundError(out_path)
    shutil.rmtree(work_afq)
    log.write.info("Finished pyAFQ")
    return out_path


def insert_pyafq(rerun: bool = False, rescan: bool = False) -> pd.DataFrame:
    """Insert data from pyAFQ tract_profiles.csv into db_adr.tbl_afq.

    Args:
        rerun: Optional, get and send pyAFQ rerun metrics instead of all.
        rescan: Optional, get and send scan-rescan metrics instead of all.

    Raises:
        EnvironmentError: Method not executed on HCC.
        FileNotFoundError: Missing pyAFQ output csv.

    Returns:
        Formatted dataframe used for insertion.

    """
    log.write.info("Inserting pyAFQ values into db_adr")

    # Verify env
    if "swan" not in platform.uname().node:
        raise EnvironmentError(
            "workflows.setub_db is written for execution on HCC."
        )

    # Identify relevant derivatives path
    deriv_dir = "/mnt/nrdstor/muncylab/nmuncy2/ADR/data_mri/derivatives"
    if rerun:
        deriv_dir = (
            "/mnt/nrdstor/muncylab/nmuncy2/ADR/data_mri/derivatives_rerun"
        )
    if rescan:
        deriv_dir = "/mnt/nrdstor/muncylab/nmuncy2/scan_rescan/derivatives"

    # Find data, insert it
    csv_path = os.path.join(deriv_dir, "afq", "tract_profiles.csv")
    if not os.path.exists(csv_path):
        raise FileNotFoundError(csv_path)

    df = pd.read_csv(csv_path)
    return database.build_afq(df, rerun=rerun, rescan=rescan)
