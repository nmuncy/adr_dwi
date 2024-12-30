"""Example of Lasso with AFQ data.

Copied from https://tractometry.org/AFQ-Insight/auto_examples/plot_age_regression.html,
installed from https://github.com/tractometry/AFQ-Insight using python=3.10.

"""

# %%
# Basic interact example ----
import os.path as op

import tensorflow as tf
from sklearn.impute import SimpleImputer
from sklearn.linear_model import Lasso
from sklearn.metrics import r2_score
from sklearn.model_selection import train_test_split

import afqinsight.nn.tf_models as nn
from afqinsight import AFQDataset
from afqinsight.datasets import download_weston_havens

# Fetch data
workdir = download_weston_havens()
dataset = AFQDataset.from_files(
    fn_nodes=op.join(workdir, "nodes.csv"),
    fn_subjects=op.join(workdir, "subjects.csv"),
    dwi_metrics=["md", "fa"],
    target_cols=["Age"],
)

# Train/test split, impute missing values
dataset_train, dataset_test = train_test_split(dataset, test_size=1 / 3)
imputer = dataset_train.model_fit(SimpleImputer(strategy="median"))
dataset_train = dataset_train.model_transform(imputer)
dataset_test = dataset_test.model_transform(imputer)

# Fit Lasso model
estimator = dataset_train.model_fit(Lasso())
y_pred = dataset_test.model_predict(estimator)
train_score = dataset_train.model_score(estimator)
test_score = dataset_test.model_score(estimator)
print("LASSO train score:", train_score)
print("LASSO test score: ", test_score)

# Convert to tensorflow dataset
tfset_train = dataset_train.as_tensorflow_dataset()
tfset_test = dataset_test.as_tensorflow_dataset()

# Use AFQ CNN to predict age
batch_size = 2
tfset_train = tfset_train.batch(8)
tfset_test = tfset_test.batch(8)

print("CNN Architecture")
model = nn.cnn_lenet(
    input_shape=(100, 40), output_activation=None, n_classes=1, verbose=True
)
model.compile(
    loss="mean_squared_error",
    optimizer=tf.keras.optimizers.Adam(learning_rate=1e-4),
    metrics=["mean_squared_error"],
)
model.fit(tfset_train, epochs=500, validation_data=tfset_test, verbose=0)
print()
print("CNN R^2 score: ", r2_score(dataset_test.y, model.predict(tfset_test)))


# %%
# Predict Age example ----
import os.path as op
import matplotlib.pyplot as plt
import numpy as np
from sklearn.model_selection import cross_validate
from afqinsight import make_afq_regressor_pipeline
from afqinsight.datasets import download_weston_havens, load_afq_data

workdir = download_weston_havens()
afqdata = load_afq_data(
    fn_nodes=op.join(workdir, "nodes.csv"),
    fn_subjects=op.join(workdir, "subjects.csv"),
    dwi_metrics=["md", "fa"],
    target_cols=["Age"],
)

# afqdata is a namedtuple. You can access it's fields using dot notation or by
# unpacking the tuple. To see all of the available fields use `afqdata._fields`
X = afqdata.X
y = afqdata.y
groups = afqdata.groups
feature_names = afqdata.feature_names
group_names = afqdata.group_names
subjects = afqdata.subjects

pipe = make_afq_regressor_pipeline(
    imputer_kwargs={"strategy": "median"},  # Use median imputation
    use_cv_estimator=True,  # Automatically determine the best hyperparameters
    scaler="standard",  # Standard scale the features before regression
    groups=groups,
    verbose=0,  # Be quiet!
    pipeline_verbosity=False,  # No really, be quiet!
    tuning_strategy="bayes",  # Use BayesSearchCV to determine optimal hyperparameters
    n_bayes_iter=10,  # Consider only this many points in hyperparameter space
    cv=3,  # Use three CV splits to evaluate each hyperparameter combination
    l1_ratio=[0.0, 1.0],  # Explore the entire range of ``l1_ratio``
    eps=5e-2,  # This is the ratio of the smallest to largest ``alpha`` value
    tol=1e-2,  # Set a lenient convergence tolerance just for this example
)

# ``pipe`` is a scikit-learn pipeline and can be used in other scikit-learn
# functions. For example, here we are doing 5-fold cross-validation using scikit
# learn's :func:`cross_validate` function.
scores = cross_validate(
    pipe, X, y, cv=5, return_train_score=True, return_estimator=True
)

# Display results
# ---------------
print(f"Mean train score: {np.mean(scores['train_score']):5.3f}")
print(f"Mean test score:  {np.mean(scores['test_score']):5.3f}")
print(f"Mean fit time:    {np.mean(scores['fit_time']):5.2f}s")
print(f"Mean score time:  {np.mean(scores['score_time']):5.2f}s")

mean_coefs = np.mean(
    np.abs([est.named_steps["estimate"].coef_ for est in scores["estimator"]]), axis=0
)
fig, ax = plt.subplots(1, 1, figsize=(8, 8))
_ = ax.plot(mean_coefs[1800:], color="black", lw=2)
_ = ax.set_xlim(0, 1800)

colors = plt.get_cmap("tab20").colors
for grp, grp_name, color in zip(groups[:18], group_names[18:], colors):
    _ = ax.axvspan(grp.min(), grp.max() + 1, color=color, alpha=0.8, label=grp_name[1])

box = ax.get_position()
ax.set_position([box.x0, box.y0 + box.height * 0.375, box.width, box.height * 0.625])

_ = ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.125), ncol=3)
_ = ax.set_ylabel(r"$\hat{\beta}$", fontsize=16)
_ = ax.set_xlabel("Group principal component", fontsize=16)
_ = ax.set_title("Group Principal Regression Coefficients (MD only)", fontsize=18)


# %%
# Predict ALS example ----
import matplotlib.pyplot as plt
import numpy as np
from groupyr.decomposition import GroupPCA
from sklearn.impute import SimpleImputer
from sklearn.model_selection import cross_validate
from afqinsight import AFQDataset, make_afq_classifier_pipeline

afqdata = AFQDataset.from_study("sarica")

# Examine the data
# ----------------
# ``afqdata`` is an ``AFQDataset`` object, with properties corresponding to
# the tractometry features and phenotypic targets.
X = afqdata.X
y = afqdata.y.astype(float)  # SGL expects float targets
groups = afqdata.groups
feature_names = afqdata.feature_names
group_names = afqdata.group_names
subjects = afqdata.subjects

# Reduce data dimensionality
# --------------------------
# Here we reduce computation time by taking the first 10 principal components of
# each feature group and performing SGL logistic regression on those components.
# If you want to train an SGL model without group PCA, set ``do_group_pca =
# False``. This will increase the number of features by an order of magnitude
# and slow down execution time.

do_group_pca = True

if do_group_pca:
    n_components = 10

    # The next three lines retrieve the group structure of the group-wise PCA
    # and store it in ``groups_pca``. We do not use the imputer or GroupPCA transformer
    # for anything else
    imputer = SimpleImputer(strategy="median")
    gpca = GroupPCA(n_components=n_components, groups=groups)
    groups_pca = gpca.fit(imputer.fit_transform(X)).groups_out_

    transformer = GroupPCA
    transformer_kwargs = {"groups": groups, "n_components": n_components}
else:
    transformer = False
    transformer_kwargs = None

# Create the classification pipeline
# ----------------------------------
# The core computational machinery is a pipeline. These operate as scikit-learn
# compatible pipelines, so we can pass them to scikit-learn functions.
# There are many options that need to be set to configure the pipeline object.
pipe = make_afq_classifier_pipeline(
    imputer_kwargs={"strategy": "median"},  # Use median imputation
    use_cv_estimator=True,  # Automatically determine the best hyperparameters
    feature_transformer=transformer,  # See note above about group PCA
    feature_transformer_kwargs=transformer_kwargs,
    scaler="standard",  # Standard scale the features before regression
    groups=(
        groups_pca if do_group_pca else groups
    ),  # SGL will use the original feature groups or the PCA feature groups depending on the choice above # noqa E501
    verbose=0,  # Be quiet!
    pipeline_verbosity=False,  # No really, be quiet!
    tuning_strategy="bayes",  # Use BayesSearchCV to determine optimal hyperparameters
    n_bayes_iter=20,  # Consider only this many points in hyperparameter space
    cv=3,  # Use three CV splits to evaluate each hyperparameter combination
    l1_ratio=[0.0, 1.0],  # Explore the entire range of ``l1_ratio``
    eps=5e-2,  # This is the ratio of the smallest to largest ``alpha`` value
    tol=1e-2,  # Set a lenient convergence tolerance just for this example
)

# Fit and cross-validate
# ----------------------
# The ``pipe`` object is a scikit-learn pipeline and can be used in other
# scikit-learn functions
scores = cross_validate(
    pipe, X, y, cv=5, return_train_score=True, return_estimator=True
)

# Display results
# ---------------
print(f"Mean train score: {np.mean(scores['train_score']):5.3f}")
print(f"Mean test score:  {np.mean(scores['test_score']):5.3f}")
print(f"Mean fit time:    {np.mean(scores['fit_time']):5.2f}s")
print(f"Mean score time:  {np.mean(scores['score_time']):5.2f}s")

mean_coefs = np.mean(
    np.abs([est.named_steps["estimate"].coef_ for est in scores["estimator"]]), axis=0
)

fig, ax = plt.subplots(1, 1, figsize=(8, 5))
_ = ax.plot(mean_coefs[:180], color="black", lw=2)
_ = ax.set_xlim(0, 180)

colors = plt.get_cmap("tab20").colors
for grp, grp_name, color in zip(groups_pca[:18], group_names, colors):
    _ = ax.axvspan(grp.min(), grp.max() + 1, color=color, alpha=0.8, label=grp_name[1])

box = ax.get_position()
_ = ax.set_position(
    [box.x0, box.y0 + box.height * 0.375, box.width, box.height * 0.625]
)

_ = ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.2), ncol=3)
_ = ax.set_ylabel(r"$\hat{\beta}$", fontsize=16)
_ = ax.set_xlabel("Group principal component", fontsize=16)
_ = ax.set_title("Group Principal Regression Coefficients (FA only)", fontsize=18)

# %%
