"""Conduct preprocessing of DWI data via FSL.

Prepare for and use topup and eddy to preprocess ADR DWI data.
Schedules an SBATCH array for all subjects specified or found
lacking the output files. Final output are found in
data-dir/derivatives/dwi_preproc.

Requires:
    - FSL to be executable in system OS.

Examples:
    preproc_dwi --subj 0003 --sess 1
    preproc_dwi --subj 000{4,6,10} --sess 2
    preproc_dwi --subj-all --array-size 40
    preproc_dwi --multi-run --subj 03298 --sess 1 --run 1 2 3

"""

import os
import sys
import glob
import platform
import time
import shutil
from datetime import datetime
import textwrap
from argparse import ArgumentParser, RawTextHelpFormatter
from adr_dwi import submit
from adr_dwi import helper

type PT = str | os.PathLike


def get_args():
    """Get and parse arguments."""
    parser = ArgumentParser(
        description=__doc__, formatter_class=RawTextHelpFormatter
    )
    parser.add_argument(
        "--array-size",
        default=30,
        help=textwrap.dedent(
            """\
            Size of sbatch array
            (default: %(default)s)
            """
        ),
        type=int,
    )
    parser.add_argument(
        "--data-dir",
        default="/mnt/nrdstor/muncylab/nmuncy2/ADR/data_mri",
        help=textwrap.dedent(
            """\
            BIDS project directory.
            (default: %(default)s)
            """
        ),
        type=str,
    )
    parser.add_argument(
        "--multi-run",
        action="store_true",
        help="Submit workflow for multiple runs in same "
        + "session, requires --subj, --sess, and --run.",
    )
    parser.add_argument(
        "--run",
        nargs="+",
        default=None,
        choices=[1, 2, 3],
        help="List of run IDs.",
        type=int,
    )
    parser.add_argument(
        "--sess",
        nargs="+",
        default=None,
        choices=[1, 2, 3],
        help="List of session IDs, or all found if not specifed.",
        type=int,
    )
    parser.add_argument(
        "--subj",
        nargs="+",
        help="List of subject IDs.",
        type=str,
    )
    parser.add_argument(
        "--subj-all",
        action="store_true",
        help="Submit data for subjects in rawdata.",
    )
    parser.add_argument(
        "--work-dir",
        default="/work/muncylab/nmuncy2/adr_dwi",
        help=textwrap.dedent(
            """\
            Intermediate directory.
            (default: %(default)s)
            """
        ),
        type=str,
    )

    if len(sys.argv) <= 1:
        parser.print_help(sys.stderr)
        sys.exit(0)

    return parser


def _find_subj_sess(
    subj_list: list,
    sess_list: list | None,
    run_list: list | None,
    raw_dir: PT,
    data_dir: PT,
) -> dict:
    """Find subjects and sessions that need preprocessing."""
    # Identify subjects and sessions for submission
    data_avail = {}
    for subj in subj_list:

        # If user specified, use requested sessions
        if sess_list or (sess_list and run_list):
            sess_out = [f"ses-{x}" for x in sess_list]

        # Find sessions without workflow output
        else:
            sess_found = [
                os.path.basename(x)
                for x in sorted(glob.glob(f"{raw_dir}/{subj}/ses-*"))
            ]
            sess_out = [
                x
                for x in sess_found
                if not os.path.exists(
                    os.path.join(
                        data_dir,
                        "derivatives",
                        "dwi_preproc",
                        subj,
                        x,
                        "dwi",
                        f"{subj}_{x}_dir-AP_desc-eddy_dwi.nii.gz",
                    )
                )
            ]
        data_avail[subj] = sess_out

    # Remove empty subjs
    data_avail = {x: y for x, y in data_avail.items() if y}
    return data_avail


def main():
    """Trigger clean rawdata."""

    # Get args
    args = get_args().parse_args()
    arr_size = args.array_size
    data_dir = args.data_dir
    work_dir = args.work_dir
    sess_list = args.sess
    subj_list = args.subj
    subj_all = args.subj_all
    multi_run = args.multi_run
    run_list = args.run

    log = helper.MakeLogger(os.path.basename(__file__))

    # Validate env
    if "swan" not in platform.uname().node:
        raise EnvironmentError("Intended for use on HCC.")
    if not shutil.which("fslroi"):
        raise EnvironmentError("Missing FSL in environment")

    # Validate args
    if multi_run:
        if not subj_list:
            raise ValueError("--multi-run requires --subj.")
        if subj_all:
            raise ValueError("--multi-run requires --subj.")
        if not sess_list:
            raise ValueError("--multi-run requires --sess.")
        if not run_list:
            raise ValueError("--multi-run requires --run.")
    if run_list and not multi_run:
        raise ValueError("--run only available with --multi-run.")

    # Setup
    work_par = os.path.join(work_dir, "dwi_preproc")
    log_dir = os.path.join(
        work_dir,
        "logs",
        f"dwi_preproc_{datetime.now().strftime('%Y%m%d_%H%M')}",
    )
    for chk_path in [work_par, log_dir]:
        if not os.path.exists(chk_path):
            os.makedirs(chk_path)

    # Find subjects or used requested subjects
    raw_dir = os.path.join(data_dir, "rawdata")
    if subj_all:
        subj_list = [
            os.path.basename(x) for x in sorted(glob.glob(f"{raw_dir}/sub-*"))
        ]
    else:
        subj_list = [f"sub-{x}" for x in subj_list]
    if not subj_list:
        raise ValueError("Empty subj_list")
    log.write.info(f"Finding sessions for following subjects: {subj_list}")

    # Find participants needing preprocessing
    data_avail = _find_subj_sess(
        subj_list, sess_list, run_list, raw_dir, data_dir
    )

    # Report and submit jobs
    subj_sess = [(x, y) for x, y_list in data_avail.items() for y in y_list]
    if multi_run:
        subj_sess = [(x, y, z) for x, y in subj_sess for z in run_list]
    log.write.info(f"Submitting jobs for: {subj_sess}")

    if multi_run:
        for subj, sess, run in subj_sess:
            _, _ = submit.sched_preproc_run(
                subj, sess, run, data_dir, work_dir, log_dir
            )
            time.sleep(3)
        return

    _, _ = submit.sched_preproc_array(
        subj_sess, arr_size, data_dir, work_dir, log_dir
    )


if __name__ == "__main__":
    main()
