from setuptools import setup, find_packages

exec(open("adr_dwi/_version.py").read())

setup(
    name="adr_dwi",
    version=__version__,  # noqa: F821
    packages=find_packages(),
    # entry_points={
    #     "console_scripts": [
    #         "iterate_rsfmri=iterate_rsfmri.entrypoint:main",
    #         "get_nki=iterate_rsfmri.clis.run_pull_nki:main",
    #         "chk_data=iterate_rsfmri.clis.run_check_data:main",
    #         "model_subj=iterate_rsfmri.clis.run_model_subj:main",
    #         "model_group=iterate_rsfmri.clis.run_model_group:main",
    #     ]
    # },
    # install_requires=[
    #     "setuptools>=65.6.3",
    #     "pandas>=1.5.2",
    #     "nibabel>=5.1.0",
    # ],
)
