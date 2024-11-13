"""Title."""

import os
import glob
import json
from adr_dwi import submit
from adr_dwi import helper

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


def cnt_nvol(subj_work: PT, subj_func: PT) -> int:
    """Return number of EPI volumes."""
    log.write.info(f"Counting vols for: {subj_func}")
    afni_head = helper.afni_sing(subj_work)
    afni_list = ["3dinfo", f"-nt {subj_func}"]
    afni_cmd = " ".join(afni_head + afni_list)
    out, err = submit.simp_subproc(afni_cmd)
    return int(out.decode("utf-8"))


class _SetSubjSess:
    """Title."""

    def __init__(self):
        """Initialize _SetSubjSess."""
        log.write.info("Initializing _SetSubjSess")
        self._subj = None
        self._sess = None
        self._data_dir = None

    @property
    def subj(self):
        """Title."""
        return self._subj

    @property
    def sess(self):
        """Title."""
        return self._sess

    @property
    def data_dir(self):
        """Title."""
        return self._data_dir

    @subj.setter
    def subj(self, val: str):
        """Title."""
        self._subj = val

    @sess.setter
    def sess(self, val: str):
        """Title."""
        self._sess = val

    @data_dir.setter
    def data_dir(self, val: PT):
        """Title."""
        self._data_dir = val


class BidsAnat(_SetSubjSess):
    """Title."""

    def __init__(self):
        """Initialize BidsAnat."""
        log.write.info("Initializing BidsAnat")
        _SetSubjSess.__init__(self)

    def _find_anat(self):
        """Title."""
        log.write.info("Finding anat files ...")
        search_dir = os.path.join(
            self._data_dir, "rawdata", self._subj, self._sess, "anat"
        )
        self._anat_list = sorted(glob.glob(f"{search_dir}/sub-*"))
        if not self._anat_list:
            raise FileNotFoundError(
                f"Expected BIDS anat rawdata at {search_dir}"
            )

    def fix_anat_names(self):
        """Title."""
        self._find_anat()
        for self._file_path in self._anat_list:
            self._file_name = os.path.basename(self._file_path)
            self._file_dir = os.path.dirname(self._file_path)
            if "acq-MEMPRAGE" in self._file_name:
                continue

            #
            file_suff = self._file_name.split("_")[-1].split(".")[0]
            fix_method = getattr(self, f"_fix_{file_suff}")
            fix_method()

    def _fix_ME(self):
        """Title."""
        log.write.info(f"BIDSifying name: {self._file_name}")
        #
        subj, sess, echo, suff = self._file_name.split("_")
        out_ext = helper.get_ext(suff)

        #
        new_name = f"{subj}_{sess}_acq-MEMPRAGE_run-1_{echo}_T1w{out_ext}"
        os.rename(self._file_path, os.path.join(self._file_dir, new_name))

    def _fix_RMS(self):
        """Title."""
        log.write.info(f"BIDSifying name: {self._file_name}")
        #
        subj, sess, _, suff = self._file_name.split("_")
        out_ext = helper.get_ext(suff)

        #
        new_name = f"{subj}_{sess}_acq-MEMPRAGErms_run-1_T1w{out_ext}"
        os.rename(self._file_path, os.path.join(self._file_dir, new_name))


class BidsDwi(_SetSubjSess):
    """Title."""

    def __init__(self):
        """Initialize BidsDwi."""
        log.write.info("Initializing BidsDwi")
        _SetSubjSess.__init__(self)

    def _find_dwi(self):
        """Title."""
        log.write.info("Finding dwi files ...")
        search_dir = os.path.join(
            self._data_dir, "rawdata", self._subj, self._sess, "dwi"
        )
        self._dwi_list = sorted(glob.glob(f"{search_dir}/sub-*"))
        if not self._dwi_list:
            raise FileNotFoundError(
                f"Expected BIDS dwi rawdata at {search_dir}"
            )

    def fix_dwi_names(self):
        """Title."""
        self._find_dwi()
        for file_path in self._dwi_list:
            file_dir = os.path.dirname(file_path)
            file_name = os.path.basename(file_path)
            if "_dir-AP" in file_name:
                continue

            #
            log.write.info(f"BIDSifying name: {file_name}")
            subj, sess, _, suff = file_name.split("_")
            dir_val = suff.split(".")[0]
            out_ext = helper.get_ext(suff)

            #
            new_name = f"{subj}_{sess}_dir-{dir_val}_dwi{out_ext}"
            os.rename(file_path, os.path.join(file_dir, new_name))


class BidsFmap(_SetSubjSess):
    """Title."""

    def __init__(self):
        """Initialize BidsFmap."""
        log.write.info("Initializing BidsFmap")
        _SetSubjSess.__init__(self)

    def _find_fmap(self, ext: str = "*"):
        """Title."""
        log.write.info("Finding fmap files ...")
        search_dir = os.path.join(
            self._data_dir, "rawdata", self._subj, self._sess, "fmap"
        )
        self._fmap_list = sorted(glob.glob(f"{search_dir}/sub-*.{ext}"))
        if not self._fmap_list:
            raise FileNotFoundError(
                f"Expected BIDS fmap rawdata at {self._raw_dir}"
            )

    def fix_fmap_names(self):
        """Title."""
        self._find_fmap()
        for file_path in self._fmap_list:
            file_name = os.path.basename(file_path)
            file_dir = os.path.dirname(file_path)
            if "_epi" in file_name:
                continue

            #
            log.write.info(f"BIDSifying name: {file_name}")
            subj, sess, _, _, _, suff = file_name.split("_")
            dir_val = suff.split(".")[0]
            out_ext = helper.get_ext(suff)

            #
            new_name = f"{subj}_{sess}_dir-{dir_val}_epi{out_ext}"
            os.rename(file_path, os.path.join(file_dir, new_name))

    def fix_fmap_intended(self):
        """Title."""
        self._find_fmap(ext="json")
        for file_path in self._fmap_list:

            #
            try:
                subj, sess, dir_val, suff = os.path.basename(file_path).split(
                    "_"
                )
            except ValueError as e:
                log.write.error(
                    "Expected BIDS dwi format: subj_sess_dir_suff.ext"
                )
                raise e
            search_dir = os.path.join(
                self._data_dir, "rawdata", subj, sess, "dwi"
            )
            dwi_list = sorted(glob.glob(f"{search_dir}/sub-*.nii.gz"))
            if not dwi_list:
                raise FileNotFoundError(f"Expected dwi files at {search_dir}")

            #
            log.write.info(
                f"Updating fmap IntendedFor: {os.path.basename(file_path)}"
            )
            with open(file_path) as jf:
                json_dict = json.load(jf)
            json_dict["IntendedFor"] = dwi_list
            with open(file_path, "w") as jf:
                json.dump(json_dict, jf)

    def fix_fmap_vols(self, vol_idx: int = 0):
        """Title."""
        self._find_fmap(ext="nii.gz")
        for file_path in self._fmap_list:

            #
            file_dir = os.path.dirname(file_path)
            file_name = os.path.basename(file_path)

            #
            num_vol = cnt_nvol(file_dir, file_path)
            if num_vol == 1:
                continue

            #
            log.write.info(f"Starting volume extration: {file_name}")
            tmp_file = os.path.join(file_dir, f"tmp_{file_name}")
            os.rename(file_path, tmp_file)
            afni_head = helper.afni_sing(file_dir)
            afni_list = [
                "3dTcat",
                f"{tmp_file}[{vol_idx}]",
                f"-prefix {file_path}",
            ]
            afni_cmd = " ".join(afni_head + afni_list)
            _, _ = submit.simp_subproc(afni_cmd)
            if not os.path.exists(file_path):
                raise FileNotFoundError(file_path)


class BidsOrg(BidsAnat, BidsDwi, BidsFmap):
    """Title."""

    def __init__(self):
        """Initialize BidsOrg."""
        log.write.info("Initializing BidsOrg")
        BidsAnat.__init__(self)
        BidsDwi.__init__(self)
        BidsFmap.__init__(self)
