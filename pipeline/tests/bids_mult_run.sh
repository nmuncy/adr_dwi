#!/bin/bash

proj_dir=/mnt/nrdstor/muncylab/nmuncy2/dwi_protocol
src_dir=${proj_dir}/sourcedata
raw_dir=${proj_dir}/rawdata

subj_id=03298
subj=sub-$subj_id
sess=ses-1
subj_raw=${raw_dir}/${subj}/$sess
mkdir -p ${subj_raw}/{dwi,fmap,anat}

subj_src=${src_dir}/SKY$subj_id

# dcm2niix \
#     -a y -b y -ba y -z y \
#     -o $subj_raw \
#     $subj_src

#
echo "Organizing anat"
for ext in json nii.gz; do
    cp ${subj_raw}/Protocol_Testing_t1_mprage*.$ext ${subj_raw}/anat/${subj}_${sess}_T1w.$ext
done

#
dwi_1shell=(22 30 38)
dwi_1shell_fmap_ap=(28 36 44)
dwi_1shell_fmap_pa=(29 37 45)

for c in ${!dwi_1shell[@]}; do
    echo "Organizing dwi 1shell $c"
    run=$(($c + 1))
    dwi_path=${subj_raw}/SKY${subj_id}_ep2d_diff_DTI_64*_${dwi_1shell[$c]}
    for src_file in ${dwi_path}.{nii.gz,bval,bvec,json}; do
        ext=${src_file#*.}
        cp $src_file ${subj_raw}/dwi/${subj}_${sess}_acq-1s134d_dir-AP_run-${run}_dwi.$ext
    done

    ap_path=${subj_raw}/SKY${subj_id}_ep2d_diff_DTI_64*_B0_AP_*${dwi_1shell_fmap_ap[$c]}
    for src_file in ${ap_path}.{nii.gz,json}; do
        ext=${src_file#*.}
        cp $src_file ${subj_raw}/fmap/${subj}_${sess}_acq-1s134d_dir-AP_run-${run}_epi.$ext
    done

    pa_path=${subj_raw}/SKY${subj_id}_ep2d_diff_DTI_64*_B0_PA_*${dwi_1shell_fmap_pa[$c]}
    for src_file in ${pa_path}.{nii.gz,json}; do
        ext=${src_file#*.}
        cp $src_file ${subj_raw}/fmap/${subj}_${sess}_acq-1s134d_dir-PA_run-${run}_epi.$ext
    done
done

#
dwi_2shell=(15 17 19)
dwi_2shell_fmap_pa=(21)
for c in ${!dwi_2shell[@]}; do
    echo "Organizing dwi 2shell $c"
    run=$(($c + 1))
    dwi_path=${subj_raw}/SKY${subj_id}_cmrr_mbep2d_DTI_2shell_66*_${dwi_2shell[$c]}
    for src_file in ${dwi_path}.{nii.gz,bval,bvec,json}; do
        ext=${src_file#*.}
        cp $src_file ${subj_raw}/dwi/${subj}_${sess}_acq-2s66d_dir-AP_run-${run}_dwi.$ext
    done
done

for c in ${!dwi_2shell_fmap_pa[@]}; do
    echo "Organizing dwi fmap 2shell $c"
    run=$(($c + 1))
    dwi_path=${subj_raw}/SKY${subj_id}_cmrr_mbep2d_DTI_2shell_66*_${dwi_2shell_fmap_pa[$c]}
    for src_file in ${dwi_path}.{nii.gz,bval,bvec,json}; do
        ext=${src_file#*.}
        cp $src_file ${subj_raw}/fmap/${subj}_${sess}_acq-2s66d_dir-PA_run-${run}_epi.$ext
    done
done

#
dwi_4shell=(6 8 10)
dwi_4shell_fmap_pa=(12)
for c in ${!dwi_4shell[@]}; do
    echo "Organizing dwi 4shell $c"
    run=$(($c + 1))
    dwi_path=${subj_raw}/SKY${subj_id}_cmrr_mbep2d_DTI_4shell_35*_${dwi_4shell[$c]}
    for src_file in ${dwi_path}.{nii.gz,bval,bvec,json}; do
        ext=${src_file#*.}
        cp $src_file ${subj_raw}/dwi/${subj}_${sess}_acq-4s35d_dir-AP_run-${run}_dwi.$ext
    done
done

for c in ${!dwi_4shell_fmap_pa[@]}; do
    echo "Organizing dwi fmap 4shell $c"
    run=$(($c + 1))
    dwi_path=${subj_raw}/SKY${subj_id}_cmrr_mbep2d_DTI_4shell_35*_${dwi_4shell_fmap_pa[$c]}
    for src_file in ${dwi_path}.{nii.gz,bval,bvec,json}; do
        ext=${src_file#*.}
        cp $src_file ${subj_raw}/fmap/${subj}_${sess}_acq-4s35d_dir-PA_run-${run}_epi.$ext
    done
done
