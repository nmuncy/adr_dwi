"""Title.

TODO

Examples:
    preproc_dwi --subj 0003 --sess 1
    preproc_dwi --subj 000{4,6,10} --sess 2
    preproc_dwi --subj-all

"""

import os
import sys
import glob
import time
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
        type=str,
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


def main():
    """Trigger clean rawdata."""

    # Get args
    args = get_args().parse_args()
    data_dir = args.data_dir
    work_dir = args.work_dir
    sess_list = args.sess
    subj_list = args.subj
    subj_all = args.subj_all

    log = helper.MakeLogger(os.path.basename(__file__))

    #
    work_par = os.path.join(work_dir, "dwi_preproc")
    log_dir = os.path.join(work_dir, "logs", "dwi_preproc")
    for chk_path in [work_par, log_dir]:
        if not os.path.exists(chk_path):
            os.makedirs(chk_path)

    #
    raw_dir = os.path.join(data_dir, "rawdata")
    if subj_all:
        subj_list = [
            os.path.basename(x) for x in sorted(glob.glob(f"{raw_dir}/sub-*"))
        ]
    else:
        subj_list = [f"sub-{x}" for x in subj_list]
    if not subj_list:
        raise ValueError("Empty subj_list")
    log.write.info(f"subj_list: {subj_list}")

    # TODO check for existing dwi_preproc files
    subj_sess = {}
    for subj in subj_list:
        if sess_list:
            sess_list = [f"ses-{x}" for x in sess_list]
        else:
            sess_list = [
                os.path.basename(x)
                for x in sorted(glob.glob(f"{raw_dir}/{subj}/ses-*"))
            ]
        subj_sess[subj] = sess_list

    #
    for subj, sess_list in subj_sess.items():
        for sess in sess_list:
            _, _ = submit.sched_preproc_dwi(
                subj, sess, data_dir, work_dir, log_dir
            )
            time.sleep(1)


if __name__ == "__main__":
    main()
