#!/bin/bash

# Setup shell environment for ADR DWI project
echo "Setting reference to pyAFQ singularity ..."
export SING_PYAFQ=/mnt/nrdstor/muncylab/nmuncy2/research_bin/sing_images/pyafq_2024-11-21.simg
if [ ! -f $SING_PYAFQ ]; then
    echo "ERROR: Please make singularity image of:"
    echo -e "\thttps://github.com/orgs/nrdg/packages/container/package/pyafq"
    echo "and store image at $SING_PYAFQ"
    exit 1
fi
export TEMPLATEFLOW_HOME=/mnt/nrdstor/muncylab/nmuncy2/research_bin/templates/templateflow

# Load necessary modules
echo "Loading FSL v6.0 ..."
module load fsl/6.0

# Load project conda environment
conda activate adr_dwi

# Print help (assumes adr_dwi package is installed in conda environment adr_dwi)
adr_dwi
