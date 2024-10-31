"""Title.

TODO

"""

import os
import pandas as pd
from adr_dwi import database


def setup_db():
    """Title."""
    raw_demo = os.path.join(
        os.environ["HOME"], "Downloads", "participant_list_for_nate.xlsx"
    )
    df = pd.read_csv()
