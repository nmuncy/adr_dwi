#!/bin/bash

# Setup shell environment for iterate rsFMRI project
echo "Setting reference to AFNI + FreeSurfer singularity ..."
export SING_AFNI=/mnt/nrdstor/muncylab/nmuncy2/research_bin/sing_images/afni_freesurfer_ub24.simg
if [ ! -f $SING_AFNI ]; then
    echo "ERROR: Please make singularity image of:"
    echo -e "\thttps://hub.docker.com/r/nmuncy/afni_freesurfer_ub24"
    echo "and store image at $SING_AFNI"
    exit 1
fi

# Load project conda environment
conda activate adr_dwi

# Print help
cat <<EOF

The package adr_dwi consists of workflows that can be accessed
from their respective CLI triggers:

    build_db    : TODO
    get_raw     : TODO

EOF
