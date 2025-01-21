"""Download NKI Rockland Archival Data.

Download anatomical and diffusion MRI data from the
NKI Rockland Archive and setup a BIDS-organized rawdata
directory.

Essentially a project-specific wrapper for methods detailed at:
    http://fcon_1000.projects.nitrc.org/indi/enhanced/neurodata.html.

Output files can be found in a BIDS-organized data-dir/rawdata.

Examples:
    get_nki -t anat dwi --age_min 17 --age_max 25 --session BAS1 --dryrun

"""

# %%
import os
import sys
import textwrap
from argparse import ArgumentParser, RawTextHelpFormatter
from adr_dwi import submit


# %%
def get_args():
    """Get and parse arguments."""
    parser = ArgumentParser(
        description=__doc__, formatter_class=RawTextHelpFormatter
    )

    parser.add_argument(
        "--age_min",
        default=17,
        help=textwrap.dedent(
            """\
            Lower threshold age, will pull data for
            participants older than (>) specified age.
            (default: %(default)s)
            """
        ),
        type=int,
    )
    parser.add_argument(
        "--age_max",
        default=100,
        help=textwrap.dedent(
            """\
            Upper threshold age, will pull data for
            participants younger than (<) specified age.
            (default: %(default)s)
            """
        ),
        type=int,
    )
    parser.add_argument(
        "--data-dir",
        default="/mnt/nrdstor/muncylab/nmuncy2/NKI/data_mri",
        help=textwrap.dedent(
            """\
            Path to parent directory of archival study
            (default: %(default)s)
            """
        ),
        type=str,
    )
    parser.add_argument(
        "--dryrun", action="store_true", help="Test download parameters"
    )
    parser.add_argument(
        "--hand",
        help="Handedness of participants, unspecified pulls both",
        choices=["L", "R"],
        type=str,
    )
    parser.add_argument(
        "--session",
        default="BAS1",
        choices=["BAS1", "BAS2", "BAS3"],
        help=textwrap.dedent(
            """\
            Session, Visit name
            (default: %(default)s)
            """
        ),
        type=str,
    )

    required_args = parser.add_argument_group("Required Arguments")
    required_args.add_argument(
        "-t",
        "--scan-type",
        nargs="+",
        choices=["anat", "dwi"],
        help="Scan type(s) to download",
        type=str,
        required=True,
    )

    if len(sys.argv) <= 1:
        parser.print_help(sys.stderr)
        sys.exit(0)

    return parser


# %%
def main():
    """Trigger NKI download workflow."""

    # Get args
    args = get_args().parse_args()
    age_min = args.age_min
    age_max = args.age_max
    dryrun = args.dryrun
    hand = args.hand
    data_dir = args.data_dir
    scan_list = args.scan_type
    sess = args.session

    # Setup data and log dirs
    log_dir = os.path.join(data_dir, "logs", "get_nki")
    if not os.path.exists(log_dir):
        os.makedirs(log_dir)

    # Submit data pull
    _, _ = submit.sched_get_nki(
        age_min,
        age_max,
        dryrun,
        hand,
        data_dir,
        scan_list,
        sess,
        log_dir,
    )


if __name__ == "__main__":
    main()

# %%
