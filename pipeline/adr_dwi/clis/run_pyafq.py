"""Generate tractographic profiles via PyAFQ.

Use PyAFQ to model preprocessed DWI via probabilistic tractography
using constrained spherical deconvolution metrics. Generates a
dataframe titled tract_profiles.csv in data-dir/derivatives/afq as
well as subject-specific tracts. The reference file bin/config.toml
is used to specify the PyAFQ API call.

Requires:
    - Global variable 'SING_PYAFQ' to hold path to singularity image of pyAFQ.

Examples:
    run_pyafq -r
    run_pyafq -r --rerun

"""

import os
import sys
import platform
from datetime import datetime
import textwrap
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
    parser.add_argument(
        "--rerun",
        action="store_true",
        help="Rerun analysis and keep derivatives separate.",
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
    work_dir = args.work_dir
    rerun = args.rerun
    run = args.run

    # Validate env
    if "swan" not in platform.uname().node:
        raise EnvironmentError("Intended for use on HCC.")
    try:
        os.environ["SING_PYAFQ"]
    except KeyError:
        raise EnvironmentError("Missing required variable SING_PYAFQ")

    # Allow for call to print help
    if not run:
        return

    # Setup
    log_dir = os.path.join(
        work_dir,
        "logs",
        f"dwi_afq_{datetime.now().strftime('%Y%m%d_%H%M')}",
    )
    if not os.path.exists(log_dir):
        os.makedirs(log_dir)

    # Report and submit jobs
    _, _ = submit.sched_pyafq(data_dir, work_dir, log_dir, rerun)


if __name__ == "__main__":
    main()
