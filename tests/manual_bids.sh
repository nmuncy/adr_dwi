#!/bin/bash

# BIDS organize data that were manually downloaded (had failed
# in main 1200 download).

subj_list=(sub-{859671,861456,877168,917255})
sess=ses-1

proj_dir=/mnt/nrdstor/muncylab/nmuncy2/HCP
src_dir=${proj_dir}/download_1200_manual
raw_dir=${proj_dir}/rawdata
deriv_dir=${proj_dir}/derivatives

dwi_src_name=(data.nii.gz bvals bvecs nodif_brain_mask.nii.gz)
dwi_dst_suff=(desc-eddy_dwi.nii.gz dwi.bval desc-eddy_dwi.eddy_rotated_bvecs desc-brain_mask.nii.gz)

for subj in ${subj_list[@]}; do
    echo $subj

    subj_src=${src_dir}/$subj
    subj_anat=${raw_dir}/${subj}/${sess}/anat
    subj_diff=${deriv_dir}/dwi_preproc/${subj}/${sess}/dwi
    mkdir -p $subj_anat
    mkdir -p $subj_diff

    r=1
    for t1_run in ${subj_src}/T1w*; do
        cp ${t1_run}/${subj#*-}_3T_T1w_MPR?.nii.gz ${subj_anat}/${subj}_${sess}_run-${r}_T1w.nii.gz
        let r+=1
    done

    c=0
    while [ $c -lt ${#dwi_src_name[@]} ]; do
        cp ${subj_src}/Diffusion/${dwi_src_name[$c]} ${subj_diff}/${subj}_${sess}_${dwi_dst_suff[$c]}
        let c+=1
    done
done
