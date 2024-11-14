"""Miscellaneous helper methods.

MakeLogger: Supply logging object.
afni_sing: Supply AFNI singularity call.
get_ext: Return extension or .nii.gz.

"""

import os
import sys
import logging

type PT = str | os.PathLike


class MakeLogger:
    """Make logging object.

    Args:
        mod_name: Name of module or logger.

    Attributes:
        write: Logging object.

    Example:
        log = helper.MakeLogger(__file__)
        log.write.info("Info Msg")
        log.write.warning("Warn Msg")
        log.write.error("Err Msg")
        log.write.critical("Crit Msg")
        log.write.debug("Debug Msg")

    """

    def __init__(self, mod_name: str):
        """Initialize MakeLogger and make logging obj as attr write."""
        self.write = logging.getLogger(mod_name)
        self.write.setLevel(logging.DEBUG)
        self._set_hand()
        self._set_form()
        self._ch.setFormatter(self._format)
        self.write.addHandler(self._ch)

    def _set_hand(self):
        """Make console handler."""
        self._ch = logging.StreamHandler(sys.stdout)
        self._ch.setLevel(logging.DEBUG)

    def _set_form(self):
        """Make formatter."""
        self._format = logging.Formatter(
            "- %(levelname)s: %(name)s (%(asctime)s) MSG: %(message)s"
        )


log = MakeLogger(os.path.basename(__file__))


def afni_sing(
    subj_work: PT,
) -> list:
    """Supply singularity call for AFNI.

    Args:
        subj_work: Location of working directory, contains all required data.

    Returns:
        list: Singularity call for AFNI with subj_work bound to /opt/home.

    Raises:
        KeyError: Missing global variable 'SING_AFNI'.

    """
    try:
        sing_afni = os.environ["SING_AFNI"]
    except KeyError as e:
        log.write.error("Missing required variable SING_AFNI")
        raise e

    return [
        "singularity run",
        "--cleanenv",
        f"--bind {subj_work}:{subj_work}",
        f"--bind {subj_work}:/opt/home",
        sing_afni,
    ]


def get_ext(suff: str) -> str:
    """Return extension or .nii.gz."""
    ext = os.path.splitext(suff)
    return ".nii.gz" if ext[1] == ".gz" else ext[1]
