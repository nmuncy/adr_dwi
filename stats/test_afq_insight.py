"""Example of Lasso with AFQ data.

Copied from https://tractometry.org/AFQ-Insight/auto_examples/plot_age_regression.html,
installed from https://github.com/tractometry/AFQ-Insight using python=3.10.

"""

# %%
import os.path as op
import matplotlib.pyplot as plt
import numpy as np
from sklearn.model_selection import cross_validate
from afqinsight import make_afq_regressor_pipeline
from afqinsight.datasets import download_weston_havens, load_afq_data


# %%
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


# %%
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


# %%
scores = cross_validate(
    pipe, X, y, cv=5, return_train_score=True, return_estimator=True
)


# %%
print(f"Mean train score: {np.mean(scores['train_score']):5.3f}")
print(f"Mean test score:  {np.mean(scores['test_score']):5.3f}")
print(f"Mean fit time:    {np.mean(scores['fit_time']):5.2f}s")
print(f"Mean score time:  {np.mean(scores['score_time']):5.2f}s")

mean_coefs = np.mean(
    np.abs([est.named_steps["estimate"].coef_ for est in scores["estimator"]]), axis=0
)


# %%
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