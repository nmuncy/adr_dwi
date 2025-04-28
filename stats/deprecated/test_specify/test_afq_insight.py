"""Title.

"""

# %%
import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from groupyr.decomposition import GroupPCA
import tensorflow as tf
from sklearn.impute import SimpleImputer
from sklearn.linear_model import Lasso
from sklearn.metrics import r2_score
from sklearn.model_selection import cross_validate
from sklearn.model_selection import train_test_split
import afqinsight.nn.tf_models as nn
from afqinsight import AFQDataset
from afqinsight import make_afq_classifier_pipeline
from afqinsight import make_afq_regressor_pipeline
from afqinsight.datasets import load_afq_data

data_dir = os.path.join(os.path.dirname(os.getcwd()), "data")


# %%
def con_group() -> list:
    # K-means subjs from group 1
    con_subj = [
        119,
        127,
        13,
        14,
        147,
        226,
        267,
        3,
        30,
        4,
        43,
        55,
        58,
        66,
        89,
        9012,
        94,
        96,
        97,
    ]
    post_subj = [x + 10000 for x in con_subj]
    con_subj = con_subj + post_subj
    con_subj = [str(x) for x in con_subj]
    return con_subj


# %%
def load_afq() -> pd.DataFrame:
    # Load AFQ data, remap column names and drop RTP session
    afq_path = os.path.join(data_dir, "df_afq.csv")
    df_afq = pd.read_csv(afq_path)
    df_afq.rename(
        columns={
            "subj_id": "subjectID",
            "scan_name": "sessionID",
            "node_id": "nodeID",
            "tract_name": "tractID",
        },
        inplace=True,
    )
    df_afq.drop("scan_date", axis=1, inplace=True)
    df_afq = df_afq[df_afq["tractID"].str.contains("Callosum")]
    return df_afq


# %%
def load_imp() -> pd.DataFrame:
    imp_path = os.path.join(data_dir, "df_impact_scan.csv")
    df_imp = pd.read_csv(imp_path)
    df_imp.rename(
        columns={
            "subj_id": "subjectID",
            "scan_name": "sessionID",
        },
        inplace=True,
    )
    return df_imp


# %%
def drop_con_subj(df: pd.DataFrame) -> pd.DataFrame:
    con_subj = con_group()
    df = df[~df["subjectID"].isin(con_subj)]
    return df


# %%
def make_afq_pre_post(
    drop_con: bool = False,
) -> tuple[pd.DataFrame, (str | os.PathLike)]:
    df_afq = load_afq()
    df_afq.drop(
        df_afq[df_afq["sessionID"] == "rtp"].index, inplace=True
    )  # focus on base vs post to start

    # Change Post subj IDs for 'unique' subjects across session
    df_afq["subjectID"] = pd.to_numeric(df_afq["subjectID"])
    df_afq.loc[df_afq["sessionID"] == "post", "subjectID"] += 10000
    df_afq["subjectID"] = df_afq["subjectID"].astype(str)

    # Drop "control" subjects from k-means clustering
    if drop_con:
        df_afq = drop_con_subj(df_afq)

    # Save to disk for read-in by load_afq_data
    afq_out = os.path.join(data_dir, "tmp_afq.csv")
    df_afq.to_csv(afq_out, index=False, header=True)
    return (df_afq, afq_out)


# %%
def make_subj_pre_post(
    drop_con: bool = False,
) -> tuple[pd.DataFrame, (str | os.PathLike)]:
    # Make subjects csv with group info (tbi status)
    df_afq, _ = make_afq_pre_post(drop_con=drop_con)
    tract_list = list(df_afq.tractID.unique())
    df_small = df_afq[(df_afq.nodeID == 10) & (df_afq.tractID == tract_list[0])].copy()
    df_small["tbi"] = 0.0
    df_small.loc[df_small["sessionID"] != "base", "tbi"] = 1.0

    # Clean and write to disk
    df_small.drop(
        ["tractID", "nodeID", "dti_fa", "dti_md", "dti_ad", "dti_rd"],
        axis=1,
        inplace=True,
    )
    subj_out = os.path.join(data_dir, "tmp_subjects.csv")
    df_small.to_csv(subj_out, index=False, header=True)
    return (df_small, subj_out)


# %%
def make_imp_post() -> tuple[pd.DataFrame, (str | os.PathLike)]:
    # Get Post impact
    df_imp = load_imp()
    df_imp = df_imp[~df_imp["sessionID"].isin(["base", "rtp"])]
    df_imp.drop(
        [
            "scan_date",
            "impact_name",
            "num_tbi",
            "impact_date",
            "diff_post",
            "diff_scan_impact",
        ],
        axis=1,
        inplace=True,
    )

    # Identify AFQ subjects
    df_afq = load_afq()
    tract_list = list(df_afq.tractID.unique())
    df_small = df_afq[
        (df_afq.nodeID == 10)
        & (df_afq.tractID == tract_list[0])
        & (df_afq.sessionID == "post")
    ].copy()
    df_small.drop(
        ["tractID", "nodeID", "dti_fa", "dti_md", "dti_ad", "dti_rd"],
        axis=1,
        inplace=True,
    )

    # Keep only subjects with both AFQ and impact data
    df_out = df_small.merge(df_imp, how="inner", on=["subjectID", "sessionID"])
    imp_out = os.path.join(data_dir, "tmp_imp.csv")
    df_out.to_csv(imp_out, index=False, header=True)
    return (df_out, imp_out)


# %%
def make_afq_post(
    drop_con: bool = False,
) -> tuple[pd.DataFrame, (str | os.PathLike)]:
    df_afq = load_afq()
    df_afq = df_afq[~df_afq["sessionID"].isin(["base", "rtp"])]

    # Drop "control" subjects from k-means clustering
    if drop_con:
        df_afq = drop_con_subj(df_afq)

    # Only keep AFQ data for subjs with AFQ + impact data
    df_imp, _ = make_imp_post()
    subj_keep = list(df_imp["subjectID"].unique())
    df_afq = df_afq[df_afq["subjectID"].isin(subj_keep)]

    # Save to disk for read-in by load_afq_data
    afq_out = os.path.join(data_dir, "tmp_afq.csv")
    df_afq.to_csv(afq_out, index=False, header=True)
    return (df_afq, afq_out)


# %%
def pred_base_post(scalar_name: str = "dti_fa"):
    # Predict base vs post ----
    #
    # Not a great model since I'm not pooling the within
    # subject variance.
    #
    # All data trained on RD: test score = 60.7%
    # All data trained on FA: test score = 49.3%
    # All data trained on MD: test score = 53.8%
    #
    # Exp group = K-means groups 2, 3.
    # Exp data trained on RD: test score = 44.9%
    # Exp data trained on FA: test score = 48.9%
    # Exp data trained on MD: test score = 45.9%

    _, afq_out = make_afq_pre_post()
    _, subj_out = make_subj_pre_post()

    afqdata = load_afq_data(
        fn_nodes=afq_out,
        fn_subjects=subj_out,
        dwi_metrics=[scalar_name],
        target_cols=["tbi"],
    )

    X = afqdata.X
    y = afqdata.y.astype(float)  # SGL expects float targets
    groups = afqdata.groups
    group_names = afqdata.group_names

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

    scores = cross_validate(
        pipe, X, y, cv=5, return_train_score=True, return_estimator=True
    )

    print(f"Mean train score: {np.mean(scores['train_score']):5.3f}")
    print(f"Mean test score:  {np.mean(scores['test_score']):5.3f}")
    print(f"Mean fit time:    {np.mean(scores['fit_time']):5.2f}s")
    print(f"Mean score time:  {np.mean(scores['score_time']):5.2f}s")

    mean_coefs = np.mean(
        np.abs([est.named_steps["estimate"].coef_ for est in scores["estimator"]]),
        axis=0,
    )

    fig, ax = plt.subplots(1, 1, figsize=(8, 5))
    _ = ax.plot(mean_coefs[:180], color="black", lw=2)
    _ = ax.set_xlim(0, 180)

    colors = plt.get_cmap("tab20").colors
    for grp, grp_name, color in zip(groups_pca[:18], group_names, colors):
        _ = ax.axvspan(
            grp.min(), grp.max() + 1, color=color, alpha=0.8, label=grp_name[1]
        )

    box = ax.get_position()
    _ = ax.set_position(
        [box.x0, box.y0 + box.height * 0.375, box.width, box.height * 0.625]
    )

    _ = ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.2), ncol=3)
    _ = ax.set_ylabel(r"$\hat{\beta}$", fontsize=16)
    _ = ax.set_xlabel("Group principal component", fontsize=16)
    _ = ax.set_title("Group Principal Regression Coefficients (FA only)", fontsize=18)


# %%
def pred_post_imp(scalar_name: str = "dti_fa", impact_meas: str = "mem_ver"):
    # Predict Post Impact measure ----

    _, imp_out = make_imp_post()
    _, afq_out = make_afq_post()

    afqdata = load_afq_data(
        fn_nodes=afq_out,
        fn_subjects=imp_out,
        dwi_metrics=[scalar_name],
        target_cols=[impact_meas],
    )

    # afqdata is a namedtuple. You can access it's fields using dot notation or by
    # unpacking the tuple. To see all of the available fields use `afqdata._fields`
    X = afqdata.X
    y = afqdata.y
    groups = afqdata.groups
    group_names = afqdata.group_names

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

    pipe = make_afq_regressor_pipeline(
        imputer_kwargs={"strategy": "median"},  # Use median imputation
        use_cv_estimator=True,  # Automatically determine the best hyperparameters
        feature_transformer=transformer,  # See note above about group PCA
        feature_transformer_kwargs=transformer_kwargs,
        scaler="standard",  # Standard scale the features before regression
        groups=(groups_pca if do_group_pca else groups),
        verbose=0,  # Be quiet!
        pipeline_verbosity=False,  # No really, be quiet!
        tuning_strategy="bayes",  # Use BayesSearchCV to determine optimal hyperparameters
        n_bayes_iter=30,  # Consider only this many points in hyperparameter space
        cv=5,  # Use three CV splits to evaluate each hyperparameter combination
        l1_ratio=[0.0, 1.0],  # Explore the entire range of ``l1_ratio``
        eps=5e-2,  # This is the ratio of the smallest to largest ``alpha`` value
        tol=1e-2,  # Set a lenient convergence tolerance just for this example
    )

    # pipe = make_afq_regressor_pipeline(
    #     imputer_kwargs={"strategy": "median"},  # Use median imputation
    #     use_cv_estimator=True,  # Automatically determine the best hyperparameters
    #     scaler="standard",  # Standard scale the features before regression
    #     groups=groups,
    #     verbose=0,  # Be quiet!
    #     pipeline_verbosity=False,  # No really, be quiet!
    #     tuning_strategy="bayes",  # Use BayesSearchCV to determine optimal hyperparameters
    #     n_bayes_iter=20,  # Consider only this many points in hyperparameter space
    #     cv=5,  # Use three CV splits to evaluate each hyperparameter combination
    #     l1_ratio=[0.0, 1.0],  # Explore the entire range of ``l1_ratio``
    #     eps=5e-2,  # This is the ratio of the smallest to largest ``alpha`` value
    #     tol=1e-2,  # Set a lenient convergence tolerance just for this example
    # )

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
        np.abs([est.named_steps["estimate"].coef_ for est in scores["estimator"]]),
        axis=0,
    )
    fig, ax = plt.subplots(1, 1, figsize=(8, 5))
    _ = ax.plot(mean_coefs, color="black", lw=2)
    _ = ax.set_xlim(0, 80)

    colors = plt.get_cmap("tab20").colors
    for grp, grp_name, color in zip(groups_pca, group_names, colors):
        _ = ax.axvspan(
            grp.min(), grp.max() + 1, color=color, alpha=0.8, label=grp_name[1]
        )

    box = ax.get_position()
    ax.set_position(
        [box.x0, box.y0 + box.height * 0.375, box.width, box.height * 0.625]
    )

    _ = ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.125), ncol=3)
    _ = ax.set_ylabel(r"$\hat{\beta}$", fontsize=16)
    _ = ax.set_xlabel("Title: X", fontsize=16)
    _ = ax.set_title(f"Title: Main: {scalar_name} p {impact_meas}", fontsize=18)


# %%
def man_post_imp(impact_meas: str = "mem_ver"):
    # Predict Post Impact measure ----

    _, imp_out = make_imp_post()
    _, afq_out = make_afq_post()

    afqdata = AFQDataset.from_files(
        fn_nodes=afq_out,
        fn_subjects=imp_out,
        dwi_metrics=["dti_fa"],
        target_cols=[impact_meas],
    )

    # Train/test split, impute missing values
    dataset_train, dataset_test = train_test_split(afqdata, test_size=1 / 3)
    imputer = dataset_train.model_fit(SimpleImputer(strategy="median"))
    dataset_train = dataset_train.model_transform(imputer)
    dataset_test = dataset_test.model_transform(imputer)

    # # Fit Lasso model
    # estimator = dataset_train.model_fit(Lasso())
    # y_pred = dataset_test.model_predict(estimator)
    # train_score = dataset_train.model_score(estimator)
    # test_score = dataset_test.model_score(estimator)
    # print("LASSO train score:", train_score)
    # print("LASSO test score: ", test_score)

    # Convert to tensorflow dataset
    tfset_train = dataset_train.as_tensorflow_dataset()
    tfset_test = dataset_test.as_tensorflow_dataset()

    # Use AFQ CNN to predict age
    batch_size = 2
    tfset_train = tfset_train.batch(8)
    tfset_test = tfset_test.batch(8)

    print("CNN Architecture")
    model = nn.cnn_lenet(
        input_shape=(100, 28), output_activation=None, n_classes=1, verbose=True
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
def main():

    # man_post_imp()
    # ["dti_fa", "dti_rd", "dti_ad", "dti_md"]

    for scalar_name in ["dti_fa", "dti_rd", "dti_ad", "dti_md"]:
        pred_base_post(scalar_name=scalar_name)
        continue
        for impact_meas in ["mem_ver", "mem_vis", "vis_mot", "rx_time"]:
            print(f"Scalar: {scalar_name}, Impact: {impact_meas}")
            pred_post_imp(scalar_name=scalar_name, impact_meas=impact_meas)


if __name__ == "__main__":
    main()

# %%
