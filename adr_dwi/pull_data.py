"""Methods for pulling and cleaning NKI data.

dl_data: Wrap download script with relevant arguments.
chg_id: Shorten subject IDs to last 5 digits.

"""

import os
import glob
import boto3  # noqa: F401
from adr_dwi import submit
from adr_dwi import helper
from adr_dwi import bin as adr_bin

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


def dl_data(
    raw_path: PT,
    pull_args: list,
    dryrun: bool,
):
    """Download NKI data and return lists of bold and physio data.

    Args:
        raw_path: Location of output BIDS directory.
        pull_args: Relevant arguments for running download script.
        dryrun: Test the download parameters.

    Raises:
        FileNotFoundError: Failure to download requested physio, rest,
            and/or task files.

    """
    log.write.info("Starting dl_data")
    if not os.path.exists(raw_path):
        os.makedirs(raw_path)
    script_path = os.path.join(
        os.path.dirname(adr_bin.__file__), "download_rockland_raw_bids_ver2.py"
    )
    pull_cmd = f"python {script_path} {' '.join(pull_args)}"
    log.write.info(f"Running pull command: {pull_cmd}")
    std_out, std_err = submit.simp_subproc(pull_cmd)
    if dryrun:
        log.write.info(f"DryRun STDOUT: {std_out}")
        log.write.info(f"DryRun STDERR: {std_err}")
        return (None, None)


def chg_id(raw_path: PT) -> dict:
    """Shorten IDs and make data BIDS compliant.

    Replace subject ID with last 5 digits of ID.

    Args:
        raw_path: Location of output BIDS directory.

    Returns:
        dict: Key = BIDS subject str, Value = List of rawdata file locations.

    """
    log.write.info("Starting chg_id")

    # Shorten IDs
    out_dict = {}
    subj_all = [
        os.path.basename(x) for x in sorted(glob.glob(f"{raw_path}/sub-*"))
    ]
    subj_switch = {x: f"sub-{x[-5:]}" for x in subj_all}
    for _long, _short in subj_switch.items():

        # Rename parent dir
        short_dir = os.path.join(raw_path, _short)
        os.rename(os.path.join(raw_path, _long), short_dir)

        # Rename files
        file_list = glob.glob(f"{short_dir}/**/{_long}_*", recursive=True)
        for _file in file_list:
            file_path, suff = _file.split(_long)
            os.rename(_file, f"{file_path}{_short}{suff}")

            # Update output dict
            if _short not in out_dict.keys():
                out_dict[_short] = [f"{file_path}{_short}{suff}"]
            else:
                out_dict[_short].append(f"{file_path}{_short}{suff}")
    log.write.info("Finished chg_id")
    return out_dict
