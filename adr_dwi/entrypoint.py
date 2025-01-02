"""Print entrypoint help."""

import adr_dwi._version as ver


def main():
    print(
        f"""

    Version : {ver.__version__}

    The package adr_dwi consists of workflows that can be accessed
    from their respective CLI triggers:

        build_db    : Clean shared Impact data and send to db_adr.
        clean_raw   : BIDSify shared ADR rawdata.
        preproc_dwi : Preprocess DWI data.
        setup_pyafq : Get preprocessed DWI data ready for running pyAFQ.
        run_pyafq   : Model DWI data via pyAFQ.

    """
    )


if __name__ == "__main__":
    main()
