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
            "setup_afq=adr_dwi.clis.run_setup_afq:main",
            "run_afq=adr_dwi.clis.run_afq:main",
        ]
    },
    include_package_data=True,
    package_data={"": ["bin/config.toml"]},
    install_requires=[
        "mysql_connector_repackaged>=0.3.1",
        "numpy>=2.1.3",
        "openpyxl>=3.1.5",
        "pandas>=2.2.3",
        "paramiko>=3.5.0",
        "PyMySQL>=1.1.1",
        "setuptools>=75.6.0",
        "sshtunnel>=0.4.0",
    ],
)
