# adr_dwi

This repository serves as the codebase for the project titled "Longitudinal Study of Concussion-Related Diffusion MRI Changes in College Athletes: Modeling Tracts via Hierarchical Generalized Additive Models", and contains a number of sections:

- [Environments](#environments): Files detailing the shell, conda, and R environments.
- [Database](#database): Description of data storage and recipes used for building local MySQL database.
- [Data Processing Pipeline](#data-processing-pipeline): Code used to build database tables, BIDSify NIfTI files, preprocess DWI data, and conduct tractography.
- [Statistics](#statistics): Code used for conducting analyses reported in the manuscript.
- [Manuscript](#manuscript): Location of drafts, bibliography, figures, and tables.


## Environments

MRI data were processed on the Holland Computing Center Cluster ([HCC](https://hcc.unl.edu/)) and statistics were conducted on a local workstation running Ubuntu 24.04. The [env](https://github.com/nmuncy/adr_dwi/tree/main/env) directory contains files for configuring (A) the shell and conda environments used in MRI processing and modeling on the HCC, and (B) the R session information used for conducting statistical analyses.

The local python package that controls data processing and modeling is found in [pipeline](https://github.com/nmuncy/adr_dwi/tree/main/pipeline) and was installed into the conda environment on the HCC. Once install, the entrypoint is accessible via `$adr_dwi`.


## Database

Data used for this project are regarded as sensitive and potentially identifiable as athletes are public figures. All data is stored and managed on internal systems at UNL, and requests for data will be considered on a case-by-base basis. All pipelines and code therefore assume local data structures and servers.

Non-MRI data are organized and stored on a local MySQL server (Gimli). Recipes for creating tables are found in [sql_db](https://github.com/nmuncy/adr_dwi/tree/main/sql_db). Tables are updated and managed through the [Data Processing Pipeline](#data-processing-pipeline), which assumes the database is structured appropriately.

MRI data are stored on the NRDStor system available through [HCC](https://hcc.unl.edu/), as are output files from statistical analyses (e.g. plots, summary tables).


## Data Processing Pipeline

Code used for preprocessing and modeling the MRI data is found in the [pipeline](https://github.com/nmuncy/adr_dwi/tree/main/pipeline) directory.


### Setup

- Install into conda environment on the HCC via `$python setup.py install`.


### Requirements

- FSL must be configured and executable in the OS
- Global variable `SING_PYAFQ`: path to singularity image of PyAFQ
- Global variable `SQL_PASS`: user password for MySQL database `db_adr`
- Global variable `RSA_WS`: RSA token for accessing workstation (Gimli)


### Usage

A number of workflows are available as sub-packages. Trigger project entrypoint via `$adr_dwi` for sub-package list:

```
(adr_dwi) [nmuncy2-hcc: ~]$ adr_dwi


    Version : 1.0.0

    The package adr_dwi consists of workflows that can be accessed
    from their respective CLI triggers:

        build_db    : Clean and send data to db_adr.
        clean_raw   : BIDSify shared ADR rawdata.
        preproc_dwi : Preprocess DWI data.
        setup_pyafq : Get preprocessed DWI data ready for running pyAFQ.
        run_pyafq   : Model DWI data via pyAFQ.
```


### Functionality

#### build_db
Clean and organize shared ImPACT and demographic data in `db_adr`. Starts with curated 'raw' XLSX files organized in project directory:

```
data_impact/
├── raw_impact_a.xlsx
├── raw_impact_b.xlsx
└── raw_participant_list.xlsx
```

Data from these 'raw' files will be populated to the relevant `db_adr` table.

#### clean_raw
Organize shared MRI Data from the Athletics Data Repository. Starts with manually curated rawdata in roughly BIDS structure. Finalized structure:

```
rawdata/sub-1234
└── ses-1
    ├── anat
    │   ├── sub-1234_ses-1_acq-MEMPRAGErms_run-1_T1w.json
    │   ├── sub-1234_ses-1_acq-MEMPRAGErms_run-1_T1w.nii.gz
    │   ├── sub-1234_ses-1_acq-MEMPRAGE_run-1_echo-1_T1w.json
    │   ├── sub-1234_ses-1_acq-MEMPRAGE_run-1_echo-1_T1w.nii.gz
    │   ├── sub-1234_ses-1_acq-MEMPRAGE_run-1_echo-2_T1w.json
    │   ├── sub-1234_ses-1_acq-MEMPRAGE_run-1_echo-2_T1w.nii.gz
    │   ├── sub-1234_ses-1_acq-MEMPRAGE_run-1_echo-3_T1w.json
    │   ├── sub-1234_ses-1_acq-MEMPRAGE_run-1_echo-3_T1w.nii.gz
    │   ├── sub-1234_ses-1_acq-MEMPRAGE_run-1_echo-4_T1w.json
    │   └── sub-1234_ses-1_acq-MEMPRAGE_run-1_echo-4_T1w.nii.gz
    ├── dwi
    │   ├── sub-1234_ses-1_dir-AP_dwi.bval
    │   ├── sub-1234_ses-1_dir-AP_dwi.bvec
    │   ├── sub-1234_ses-1_dir-AP_dwi.json
    │   └── sub-1234_ses-1_dir-AP_dwi.nii.gz
    ├── fmap
    │   ├── sub-1234_ses-1_dir-AP_epi.json
    │   ├── sub-1234_ses-1_dir-AP_epi.nii.gz
    │   ├── sub-1234_ses-1_dir-PA_epi.json
    │   └── sub-1234_ses-1_dir-PA_epi.nii.gz
    └── sub-1234_ses-1_scans.tsv
```


#### preproc_dwi
Preprocess and prepare DWI data for tractography. This includes a few steps:

1. Extract b0 volumes from AP and PA fmaps
2. Combine AP+PA b0 files
3. Write acquistion parameters for AP+PA file
4. Calculate distortion correction via topup
5. Generate a brain mask
6. Build an index file
7. Preprocess DWI via eddy

Data will be saved to the `dwi_preproc` directory:

```
dwi_preproc/sub-1234
└── ses-1
    └── dwi
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_command_txt
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_mbs_first_order_fields.nii.gz
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_movement_over_time
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_movement_rms
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_outlier_free_data.nii.gz
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_outlier_map
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_outlier_n_sqr_stdev_map
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_outlier_n_stdev_map
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_outlier_report
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_parameters
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_post_eddy_shell_alignment_parameters
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_post_eddy_shell_PE_translation_parameters
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_restricted_movement_rms
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_rotated_bvecs
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.eddy_values_of_all_input_parameters
        ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.nii.gz
        ├── sub-1234_ses-1_dir-AP_dwi.bval
        └── sub-1234_ses-1_dir-AP_dwi.json
```


#### setup_pyafq

Organize preprocessed data into a format compatible with PyAFQ. The following directory structure will be generated in the 'work' location from preprocessed DWI data:

```
dwi_afq/
├── config.toml
├── dataset_description.json
└── sub-1234
    └── ses-1
        └── dwi
            ├── sub-1234_ses-1_dir-AP_desc-brain_mask.nii.gz
            ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.bvec
            ├── sub-1234_ses-1_dir-AP_desc-eddy_dwi.nii.gz
            ├── sub-1234_ses-1_dir-AP_dwi.bval
            └── sub-1234_ses-1_dir-AP_dwi.json
```

#### run_pyafq

Using the output of `setup_pyafq`, this sub-package will trigger PyAFQ to generate tract profiles for each subject and session. The `config.toml` is configurable and found in the `bin` location of the package.

Output files wiarel organized in the typical PyAFQ fashion and written to the project directory. These data can also be added to the `db_adr` database by triggering `workflows.insert_pyafq()`.


### Notes

- A singularity image was built for PyAFQ for use on the HCC, see [here](https://tractometry.org/pyAFQ/howto/installation_guide.html) for help.


## Statistics

R code used to conduct analyses is organized in the [stats](https://github.com/nmuncy/adr_dwi/tree/main/stats) directory. The various analyses are organized in the `workflows.R` module, which coordinates the relevant functions from `resources`. The script `model_dwi.R` conducts the workflows reported in the manuscript. Output R objects, plots, and summary tables are written to the project file structure (NRDStor).


## Manuscript

Original drafts of the mansucript were written in LaTex, the [manuscript](https://github.com/nmuncy/adr_dwi/tree/main/manuscript) directory contains these Tex files as well as figures, tables, and bibliography.
