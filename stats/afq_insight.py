"""Title."""

import sys
import os.path as op

import tensorflow as tf
from sklearn.impute import SimpleImputer
from sklearn.linear_model import Lasso
from sklearn.metrics import r2_score
from sklearn.model_selection import train_test_split

import afqinsight.nn.tf_models as nn
from afqinsight import AFQDataset
from afqinsight.datasets import download_weston_havens


def main():
    pass


if __name__ == "__main__":
    # Require proj env
    env_found = [x for x in sys.path if "afq_insight" in x]
    if not env_found:
        print("\nERROR: missing required project environment 'afq_insight'.")
        sys.exit(1)
    main()
