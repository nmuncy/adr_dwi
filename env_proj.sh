#!/bin/bash

# Setup shell environment for ADR DWI project
echo "Setting reference to AFNI + FreeSurfer singularity ..."
export SING_AFNI=/mnt/nrdstor/muncylab/nmuncy2/research_bin/sing_images/afni_freesurfer_ub24.simg
if [ ! -f $SING_AFNI ]; then
    echo "ERROR: Please make singularity image of:"
    echo -e "\thttps://hub.docker.com/r/nmuncy/afni_freesurfer_ub24"
    echo "and store image at $SING_AFNI"
    exit 1
fi

#
echo "Loading MRTrix v3.0, ANTs v2.5, FSL v6.0 ..."
# module load mrtrix3/3.0
# module load ants/2.5
module load fsl/6.0

# Load project conda environment
conda activate adr_dwi

# Print help
adr_dwi
