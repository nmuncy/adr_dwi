"""Send ImPACT and PyAFQ data to MySQL database.

Assumes pre-built database and tables.

Example:
    srun --cpus-per-task 1 --mem 4G --pty $SHELL -i # Schedule SLURM resources
    build_db --insert-impact
    build_db --insert-pyafq
    build_db --insert-pyafq-rerun

"""

import os
import sys
import platform
from argparse import ArgumentParser, RawTextHelpFormatter
from adr_dwi import workflows


def get_args():
    """Get and parse arguments."""
    parser = ArgumentParser(
        description=__doc__, formatter_class=RawTextHelpFormatter
    )
    parser.add_argument(
        "--insert-impact",
        action="store_true",
        help="Clean and send shared IMPACT data to db_adr.",
    )
    parser.add_argument(
        "--insert-pyafq",
        action="store_true",
        help="Send pyAFQ metrics to db_adr.",
    )
    parser.add_argument(
        "--insert-pyafq-rerun",
        action="store_true",
        help="Send pyAFQ rerun metrics to db_adr.",
    )

    if len(sys.argv) <= 1:
        parser.print_help(sys.stderr)
        sys.exit(0)

    return parser


def _check_env():
    """Raise EnvironmentError if main not running on scheduled resources."""
    if "swan" not in platform.uname().node:
        raise EnvironmentError("build_db is written for execution on HCC.")

    # Check for scheduled resources
    msg_rsc = (
        "Please execute build_db with SLURM scheduled resources: "
        + "--cpus-per-task 1 --mem 4G"
    )
    try:
        num_cpu = os.environ["SLURM_CPUS_ON_NODE"]
        if num_cpu != "1":
            raise EnvironmentError(msg_rsc)
    except KeyError:
        raise EnvironmentError(msg_rsc)


def main():
    """Coordinate insert workflows."""

    args = get_args().parse_args()
    _check_env()

    if args.insert_impact:
        workflows.insert_impact()
    if args.insert_pyafq:
        _ = workflows.insert_pyafq()
    if args.insert_pyafq_rerun:
        _ = workflows.insert_pyafq(rerun=True)


if __name__ == "__main__":
    main()
