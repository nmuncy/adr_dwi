"""Methods for interacting with SLURM scheduler and subprocesses.

simp_subproc: Submit bash command as subprocess.
sched_subproc: Schedule bash command with SLURM.

TODO

"""

import os
import sys
import subprocess
import textwrap
from adr_dwi import helper

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


def simp_subproc(bash_cmd: str, wait: bool = True) -> tuple:
    """Spawn simple subprocess and return stdout/err.

    Args:
        bash_cmd: Bash syntax to be submitted to subprocess.
        wait: Hang until subprocess completes.

    Returns:
        tuple: stdout/err of subprocess.

    """
    log.write.info(f"Submitting subprocess: {bash_cmd}")
    h_sp = subprocess.Popen(
        bash_cmd,
        shell=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    h_out, h_err = h_sp.communicate()
    if wait:
        h_sp.wait()
    return (h_out, h_err)


def sched_subproc(
    bash_cmd: str,
    job_name: str,
    log_dir: PT,
    wait: bool = True,
    num_hours: int = 1,
    num_cpus: int = 1,
    mem_gig: int = 4,
) -> tuple:
    """Schedule subprocess with SLURM manager.

    Args:
        bash_cmd: Bash sytnax to be submitted to subprocess.
        job_name: Name of job for scheduler.
        log_dir: Location for writing stdout/err.
        wait: Hang until subprocess completes.
        num_hours: Requested walltime.
        num_cpus: Requested number of processors.
        mem_gig: Requested GB memory.

    Returns:
        tuple: stdout/err of subprocess.

    """
    # Build sbatch head
    sbatch_head = [
        "sbatch",
        f"-J {job_name}",
        f"-t {num_hours}:00:00",
        f"--cpus-per-task={num_cpus}",
        f"--mem={mem_gig}G",
        f"-o {log_dir}/out_{job_name}.log",
        f"-e {log_dir}/err_{job_name}.log",
    ]
    if wait:
        sbatch_head.append("--wait")

    # Submit command
    sbatch_cmd = " ".join(sbatch_head + [f""" --wrap="{bash_cmd}" """])
    return simp_subproc(sbatch_cmd, wait=wait)


def sched_clean_rawdata(
    data_dir: PT,
    log_dir: PT,
) -> tuple:
    """Schedule workflows.clean_rawdata().

    Args:
        data_dir: BIDS data location.
        log_dir: Location for writing stdout/err.

    Returns:
        tuple: stdout/err of subprocess.

    """
    # Write parent python script
    sbatch_cmd = f"""\
        #!/bin/env {sys.executable}

        #SBATCH --job-name=cl_raw
        #SBATCH --output={log_dir}/cl_raw.log
        #SBATCH --time=10:00:00
        #SBATCH --mem=6G

        from adr_dwi import workflows

        workflows.clean_rawdata("{data_dir}")

    """
    sbatch_cmd = textwrap.dedent(sbatch_cmd)
    py_script = f"{log_dir}/run_clean_rawdata.py"
    with open(py_script, "w") as ps:
        ps.write(sbatch_cmd)
    log.write.info(f"Wrote script: {py_script}")

    # Execute script
    h_out, h_err = simp_subproc(f"sbatch {py_script}", wait=False)
    log.write.info(h_out.decode("utf-8"))
    return (h_out, h_err)


def sched_preproc_dwi(
    subj: str,
    sess: str,
    data_dir: PT,
    work_dir: PT,
    log_dir: PT,
):
    """Schedule workflows.preproc_dwi.

    Args:
        subj: BIDS subject ID.
        sess: BIDS session ID.
        data_dir: BIDS data location.
        log_dir: Location for writing stdout/err.

    Returns:
        tuple: stdout/err of subprocess.

    """
    sbatch_cmd = f"""\
        #!/bin/env {sys.executable}

        #SBATCH --output={log_dir}/dwi_{subj[4:]}_{sess[4:]}.log
        #SBATCH --time=12:00:00
        #SBATCH --mem=4G

        from adr_dwi import workflows

        workflows.preproc_dwi(
            "{subj}",
            "{sess}",
            "{data_dir}",
            "{work_dir}",
            "{log_dir}",
        )

    """
    sbatch_cmd = textwrap.dedent(sbatch_cmd)

    # Write as script
    py_script = f"{log_dir}/dwi_{subj[4:]}_{sess[4:]}.py"
    with open(py_script, "w") as ps:
        ps.write(sbatch_cmd)
    log.write.info(f"Wrote script: {py_script}")

    # Execute script
    h_out, h_err = simp_subproc(f"sbatch {py_script}", wait=False)
    log.write.info(h_out.decode("utf-8"))
    return (h_out, h_err)
