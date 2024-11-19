"""Print entrypoint help."""

import adr_dwi._version as ver


def main():
    print(
        f"""

    Version : {ver.__version__}

    The package adr_dwi consists of workflows that can be accessed
    from their respective CLI triggers:

        build_db    : TODO
        clean_raw   : TODO
        preproc_dwi : TODO

    """
    )


if __name__ == "__main__":
    main()
