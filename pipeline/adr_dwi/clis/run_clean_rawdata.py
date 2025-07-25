"""Fully BIDSify rawdata.

Data copied from attic/barbey/shared/ADR is nearly BIDS compliant,
this will finish the BIDS-ification process for anat, dwi, and
fmap files copied to data-dir/rawdata.

Notes:
    - To be used after manual copying (scp) of data from attic to nrdstor.
    - Assumes the extra DWI and SWI files have manually been removed from
        nrdstor rawdata.

Example:
    clean_raw -r

"""

import os
import sys
import textwrap
import platform
from argparse import ArgumentParser, RawTextHelpFormatter
from adr_dwi import submit


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

    required_args = parser.add_argument_group("Required Arguments")
    required_args.add_argument(
        "-r",
        "--run",
        action="store_true",
        help="Run the workflow.",
        required=True,
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
    run = args.run

    # Validate env
    if "swan" not in platform.uname().node:
        raise EnvironmentError("Intended for use on HCC.")

    # Allow for call to print help
    if not run:
        return

    # Setup data and log dirs
    log_dir = os.path.join(data_dir, "logs", "clean_rawdata")
    if not os.path.exists(log_dir):
        os.makedirs(log_dir)

    # Submit data check
    if run:
        _, _ = submit.sched_clean_rawdata(data_dir, log_dir)


if __name__ == "__main__":
    main()
