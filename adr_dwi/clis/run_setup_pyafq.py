"""Make and aggregate files needed to run pyAFQ.

Setup working location with all files required to run pyAFQ, including
BIDS dataset_description.json, config.toml, preprocessed DWI files,
and brain masks. Schedules an SBATCH array for all subjects specified or
found to have required preprocessed files. Output will be found in
work-dir/dwi_afq.

Notes:
    - For HCP data, the directory path must contain "HCP".

Requires:
    - FSL to be executable in system OS.

Examples:
    setup_pyafq --subj 0003 --sess 1
    setup_pyafq --subj-all


"""

import os
import sys
import glob
import platform
import shutil
from datetime import datetime
import textwrap
from argparse import ArgumentParser, RawTextHelpFormatter
from adr_dwi import submit
from adr_dwi import helper


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
        help="Submit data for subjects with dwi_preproc output.",
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

    log = helper.MakeLogger(os.path.basename(__file__))

    # Validate env
    if "swan" not in platform.uname().node:
        raise EnvironmentError("Intended for use on HCC.")
    if not shutil.which("bet"):
        raise EnvironmentError("Missing FSL in environment")

    # Setup
    work_par = os.path.join(work_dir, "dwi_afq")
    log_dir = os.path.join(
        work_dir,
        "logs",
        f"dwi_afq-setup_{datetime.now().strftime('%Y%m%d_%H%M')}",
    )
    for chk_path in [work_par, log_dir]:
        if not os.path.exists(chk_path):
            os.makedirs(chk_path)

    # Find subjects or used requested subjects
    deriv_dir = os.path.join(data_dir, "derivatives", "dwi_preproc")
    if subj_all:
        subj_list = [
            os.path.basename(x)
            for x in sorted(glob.glob(f"{deriv_dir}/sub-*"))
        ]
    else:
        subj_list = [f"sub-{x}" for x in subj_list]
    if not subj_list:
        raise ValueError("Empty subj_list")
    log.write.info(f"Finding sessions for following subjects: {subj_list}")

    # Identify subjects and sessions for submission
    data_avail = {}
    for subj in subj_list:

        # Identify available sessions
        if sess_list:

            # If use specified, use requested sessions
            sess_out = [f"ses-{x}" for x in sess_list]
        else:

            # Find sessions without workflow output
            sess_found = [
                os.path.basename(x)
                for x in sorted(glob.glob(f"{deriv_dir}/{subj}/ses-*"))
            ]

            sess_out = [
                x
                for x in sess_found
                if not os.path.exists(
                    os.path.join(
                        work_dir,
                        "dwi_afq",
                        subj,
                        x,
                        "dwi",
                        f"{subj}_{x}_dir-AP_desc-eddy_dwi.nii.gz",
                    )
                )
            ]
        data_avail[subj] = sess_out

    # Remove subjects who do not have/need any sessions
    data_avail = {x: y for x, y in data_avail.items() if y}

    # Report and submit jobs
    subj_sess = [(x, y) for x, y_list in data_avail.items() for y in y_list]
    log.write.info(f"Submitting jobs for: {subj_sess}")
    args = [subj_sess, arr_size, data_dir, work_dir, log_dir]
    if "HCP" in data_dir:
        args.append(True)
    _, _ = submit.sched_setup_pyafq_array(*args)


if __name__ == "__main__":
    main()
