from setuptools import setup, find_packages

exec(open("adr_dwi/_version.py").read())

setup(
    name="adr_dwi",
    version=__version__,  # noqa: F821
    packages=find_packages(),
    entry_points={
        "console_scripts": [
            "adr_dwi=adr_dwi.entrypoint:main",
            "clean_raw=adr_dwi.clis.run_clean_rawdata:main",
            "preproc_dwi=adr_dwi.clis.run_preproc_dwi:main",
        ]
    },
    # install_requires=[
    #     "setuptools>=65.6.3",
    #     "pandas>=1.5.2",
    #     "nibabel>=5.1.0",
    # ],
)
