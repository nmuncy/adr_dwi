"""Title.

TODO

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
