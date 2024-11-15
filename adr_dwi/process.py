"""Methods for processing data.

cnt_nvol: Determine number of EPI volumes.
BidsAnat: BIDSify anat files.
BidsDwi: BIDSify dwi files.
BidsFmap: BIDSify fmap files.
BidsOrg: Organize BidsAnat, BidsDwi, and BidsFmap into one class.

"""

import os
import glob
import json
import shutil
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
    """Provide setters and getters for subj, sess, and data_dir."""

    def __init__(self):
        """Initialize _SetSubjSess."""
        log.write.info("Initializing _SetSubjSess")
        self._subj = None
        self._sess = None
        self._data_dir = None

    @property
    def subj(self):
        """Get subj attr."""
        return self._subj

    @property
    def sess(self):
        """Get sess attr."""
        return self._sess

    @property
    def data_dir(self):
        """Get data_dir attr."""
        return self._data_dir

    @subj.setter
    def subj(self, val: str):
        """Set subj attr."""
        self._subj = val

    @sess.setter
    def sess(self, val: str):
        """Set sess attr."""
        self._sess = val

    @data_dir.setter
    def data_dir(self, val: PT):
        """Set data_dir attr."""
        self._data_dir = val


class BidsAnat(_SetSubjSess):
    """BIDSify anat directory.

    Assumes local attic organization of anat containing MEMPRAGE files and
    SWI files removed.

    Example:
        bids_anat = process.BidsAnat()
        bids_anat.subj = "sub-0001"
        bids_anat.sess = "ses-1"
        bids_anat.data_dir = "/path/to/BIDS/dir"
        bids_anat.fix_anat_names()

    """

    def __init__(self):
        """Initialize BidsAnat."""
        log.write.info("Initializing BidsAnat")
        _SetSubjSess.__init__(self)

    def _find_anat(self):
        """Find anat files."""
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
        """Use file suffices to fix ME and RMS filenames."""
        self._find_anat()
        log.write.info("BIDSifying anat files")
        for file_path in self._anat_list:
            self._file_name = os.path.basename(file_path)
            file_dir = os.path.dirname(file_path)

            # Rename by calling relevant method
            file_suff = self._file_name.split("_")[-1].split(".")[0]
            if file_suff == "T1w":
                continue
            get_name = getattr(self, f"_fix_{file_suff}")
            new_name = get_name()
            os.rename(file_path, os.path.join(file_dir, new_name))

    def _fix_ME(self) -> str:
        """Return BIDS ME name."""
        subj, sess, echo, suff = self._file_name.split("_")
        out_ext = helper.get_ext(suff)
        return f"{subj}_{sess}_acq-MEMPRAGE_run-1_{echo}_T1w{out_ext}"

    def _fix_RMS(self) -> str:
        """Return BIDS RMS name."""
        subj, sess, _, suff = self._file_name.split("_")
        out_ext = helper.get_ext(suff)
        return f"{subj}_{sess}_acq-MEMPRAGErms_run-1_T1w{out_ext}"


class BidsDwi(_SetSubjSess):
    """BIDSify dwi directory.

    Assumes local attic organization of DWI containing with b0 and
    b1000t files removed.

    Example:
        bids_dwi = process.BidsDwi()
        bids_dwi.subj = "sub-0001"
        bids_dwi.sess = "ses-1"
        bids_dwi.data_dir = "/path/to/BIDS/dir"
        bids_dwi.fix_dwi_names()

    """

    def __init__(self):
        """Initialize BidsDwi."""
        log.write.info("Initializing BidsDwi")
        _SetSubjSess.__init__(self)

    def _find_dwi(self):
        """Find dwi files."""
        log.write.info("Finding dwi files ...")
        search_dir = os.path.join(
            self._data_dir, "rawdata", self._subj, self._sess, "dwi"
        )
        self._dwi_list = sorted(glob.glob(f"{search_dir}/sub-*"))

    def fix_dwi_names(self):
        """BIDSify dwi name."""
        log.write.info("BIDSifying dwi files")
        self._find_dwi()
        if not self._dwi_list:
            log.write.info("No dwi files detected")
            return

        for file_path in self._dwi_list:
            file_dir = os.path.dirname(file_path)
            file_name = os.path.basename(file_path)
            if "_dir-AP" in file_name:
                continue

            # Rename file
            subj, sess, _, suff = file_name.split("_")
            dir_val = suff.split(".")[0]
            out_ext = helper.get_ext(suff)
            new_name = f"{subj}_{sess}_dir-{dir_val}_dwi{out_ext}"
            os.rename(file_path, os.path.join(file_dir, new_name))


class BidsFmap(_SetSubjSess):
    """BIDSify fmap directory.

    Assumes local attic organization of fmap files.

    Notes:
        - Order of method execution is important.
        - Requires BIDSified DWI files.

    Example:
        bids_fmap = process.BidsFmap()
        bids_fmap.subj = "sub-0001"
        bids_fmap.sess = "ses-1"
        bids_fmap.data_dir = "/path/to/BIDS/dir"
        bids_fmap.fix_fmap_names()
        bids_fmap.fix_fmap_intended()
        bids_fmap.fix_fmap_vols()

    """

    def __init__(self):
        """Initialize BidsFmap."""
        log.write.info("Initializing BidsFmap")
        _SetSubjSess.__init__(self)

    def _find_fmap(self, ext: str = "*"):
        """Find fmap files given extension."""
        log.write.info(f"Finding fmap.{ext} files ...")
        search_dir = os.path.join(
            self._data_dir, "rawdata", self._subj, self._sess, "fmap"
        )
        self._fmap_list = sorted(glob.glob(f"{search_dir}/sub-*.{ext}"))

    def fix_fmap_names(self):
        """BIDSify fmap name."""
        log.write.info("BIDSifying fmap files")
        self._find_fmap()
        if not self._fmap_list:
            log.write.info("No fmap files detected")
            return

        for file_path in self._fmap_list:
            file_name = os.path.basename(file_path)
            file_dir = os.path.dirname(file_path)
            if "_epi" in file_name:
                continue

            # Rename file
            subj, sess, _, _, _, suff = file_name.split("_")
            dir_val = suff.split(".")[0]
            out_ext = helper.get_ext(suff)
            new_name = f"{subj}_{sess}_dir-{dir_val}_epi{out_ext}"
            os.rename(file_path, os.path.join(file_dir, new_name))

    def fix_fmap_intended(self):
        """Add IntendedFor field to JSON sidecar."""
        log.write.info("BIDSifying fmap files - updating IntendedFor")
        self._find_fmap(ext="json")
        if not self._fmap_list:
            log.write.info("No fmap files detected")
            return

        for file_path in self._fmap_list:

            # Require BIDS file names
            try:
                subj, sess, dir_val, suff = os.path.basename(file_path).split(
                    "_"
                )
            except ValueError as e:
                log.write.error(
                    "Expected BIDS dwi format: subj_sess_dir_suff.ext"
                )
                raise e

            # Find DWI files
            search_dir = os.path.join(
                self._data_dir, "rawdata", subj, sess, "dwi"
            )
            dwi_list = sorted(glob.glob(f"{search_dir}/sub-*.nii.gz"))
            if not dwi_list:
                log.write.info("Did not find corresponding DWI files")
                continue

            # Update JSON
            with open(file_path) as jf:
                json_dict = json.load(jf)
            json_dict["IntendedFor"] = dwi_list
            with open(file_path, "w") as jf:
                json.dump(json_dict, jf)

    def fix_fmap_vols(self, vol_idx: int = 0):
        """Extract volume vol_idx from fmap."""
        log.write.info("BIDSifying fmap files - removing extra volumes")
        self._find_fmap(ext="nii.gz")
        if not self._fmap_list:
            log.write.info("No fmap files detected")
            return

        for file_path in self._fmap_list:

            # Determine if extraction is needed
            file_dir = os.path.dirname(file_path)
            file_name = os.path.basename(file_path)
            num_vol = cnt_nvol(file_dir, file_path)
            if num_vol == 1:
                continue

            # Extract volume
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
            os.remove(tmp_file)


class BidsOrg(BidsAnat, BidsDwi, BidsFmap):
    """Combine BIDS classes into one for centralized methods.

    Inherits BidsAnat, BidsDwi, and BidsFmap.

    """

    def __init__(self):
        """Initialize BidsOrg."""
        log.write.info("Initializing BidsOrg")
        BidsAnat.__init__(self)
        BidsDwi.__init__(self)
        BidsFmap.__init__(self)


class FslTopup:
    """Title."""

    def __init__(self, subj: str, sess: str, log_dir: PT):
        """Initialize FslTopup."""
        log.write.info("Initializing FslTopup")
        self._subj = subj
        self._sess = sess
        self._log_dir = log_dir

    def extract_b0(self, in_path: PT, out_name: str) -> PT:
        """Extract b0 volume as new file via fslroi.

        Args:
            in_path: Location of input file.
            out_name: Desired output prefix.

        Returns:
            Location of output file (same dir as in_path).

        Raises:
            FileNotFoundError: Missing output file.

        """
        log.write.info(f"Extracting b0: {os.path.basename(in_path)}")
        out_dir = os.path.dirname(in_path)
        out_path = os.path.join(out_dir, f"{out_name}.nii.gz")
        if os.path.exists(out_path):
            return out_path

        fsl_cmd = ["fslroi", in_path, os.path.join(out_dir, out_name), "0 1"]
        out, err = submit.simp_subproc(" ".join(fsl_cmd))
        if not os.path.exists(out_path):
            log.write.debug(f"STDOUT: {out}")
            log.write.debug(f"STDERR: {err}")
            raise FileNotFoundError(out_path)
        return out_path

    def combine_b0(
        self, ap_path: PT, pa_path: PT, out_name: str = "tmp_AP_PA_b0"
    ) -> PT:
        """Combine AP and PA b0 files via fslmerge.

        Args:
            ap_path: Location of AP b0 file.
            pa_path: Location of PA b0 file.
            out_name: Optional, desired output prefix.

        Returns:
            Location of output file (same dir as ap_path).

        Raises:
            FileNotFoundError: Missing output file.

        """
        log.write.info("Combining b0 files")
        out_dir = os.path.dirname(ap_path)
        out_path = os.path.join(out_dir, f"{out_name}.nii.gz")
        if os.path.exists(out_path):
            return out_path

        #
        fsl_cmd = [
            "fslmerge",
            f"-t {os.path.join(out_dir, out_name)}",
            ap_path,
            pa_path,
        ]
        out, err = submit.simp_subproc(" ".join(fsl_cmd))
        if not os.path.exists(out_path):
            log.write.debug(f"STDOUT: {out}")
            log.write.debug(f"STDERR: {err}")
            raise FileNotFoundError(out_path)
        return out_path

    def _is_empty(self, file_path: PT) -> bool:
        """Title."""
        return os.stat(file_path).st_size == 0

    def acq_param(
        self, json_path: PT, out_name: str = "tmp_acq_param.txt"
    ) -> PT:
        """Title."""
        log.write.info("Writing acq_param.txt")
        out_dir = os.path.dirname(json_path)
        out_path = os.path.join(out_dir, out_name)
        if os.path.exists(out_path) and not self._is_empty(out_path):
            return out_path

        #
        with open(json_path) as jf:
            json_dict = json.load(jf)

        line_a = f"0 1 0 {json_dict['TotalReadoutTime']}\n"
        line_b = f"0 -1 0 {json_dict['TotalReadoutTime']}\n"
        with open(out_path, "w") as f:
            f.write(line_a)
            f.write(line_b)

        if self._is_empty(out_path):
            raise ValueError(f"Empty file: {out_path}")
        return out_path

    def run_topup(
        self, ap_pa_b0: PT, acq_param: PT, out_name: str = "tmp_topup"
    ) -> PT:
        """Title."""
        log.write.info("Running topup")
        out_dir = os.path.dirname(ap_pa_b0)
        out_path = os.path.join(out_dir, f"{out_name}_fieldcoef.nii.gz")
        if os.path.exists(out_path):
            return out_path

        fsl_cmd = [
            "topup",
            f"--imain={ap_pa_b0}",
            f"--datain={acq_param}",
            "--config=b02b0.cnf",
            f"--out={os.path.join(out_dir, out_name)}",
        ]
        job_name = f"topup_{self._subj[4:]}_{self._sess[4:]}"
        out, err = submit.sched_subproc(
            " ".join(fsl_cmd), job_name, self._log_dir
        )
        if not os.path.exists(out_path):
            raise FileNotFoundError(out_path)
        return out_path


class DwiPreproc(FslTopup):
    """Title."""

    def __init__(
        self, subj: str, sess: str, work_dir: PT, data_dir: PT, log_dir: PT
    ):
        """Initialize FslPreproc."""
        log.write.info("Initializing FslPreproc")
        if not shutil.which("fslroi"):
            raise EnvironmentError("Missing FSL in environment")

        self._subj = subj
        self._sess = sess
        self._work_dir = work_dir
        self._data_dir = data_dir
        self._work_dir = work_dir

        super().__init__(subj, sess, log_dir)

    def setup(self) -> list:
        """Title."""
        log.write.info("Running setup")
        self._subj_data = os.path.join(
            self._data_dir, "rawdata", self._subj, self._sess
        )
        self._subj_work = os.path.join(
            self._work_dir, "dwi_preproc", self._subj, self._sess, "dwi"
        )
        self._subj_deriv = os.path.join(
            self._data_dir,
            "derivatives",
            "dwi_preproc",
            self._subj,
            self._sess,
            "dwi",
        )
        for chk_path in [self._subj_work, self._subj_deriv]:
            if not os.path.exists(chk_path):
                os.makedirs(chk_path)

        dwi_dict = self._get_dwi()
        fmap_dict = self._get_fmap()
        return (dwi_dict, fmap_dict)

    def _get_dwi(self) -> dict:
        """Title."""

        # Get DWI files
        subj_dwi = os.path.join(self._subj_data, "dwi")
        if not glob.glob(f"{subj_dwi}/sub*"):
            raise FileNotFoundError(f"Expected dwi data at {subj_dwi}")
        _, _ = submit.simp_subproc(f"cp {subj_dwi}/sub* {self._subj_work}")
        dwi_list = sorted(glob.glob(f"{self._subj_work}/sub-*_dwi.*"))
        if len(dwi_list) != 4:
            raise FileNotFoundError(f"Expected dwi data at {self._subj_work}")

        dwi_dict = {}
        for dwi_path in dwi_list:
            dwi_ext = os.path.splitext(dwi_path)[1]
            key = "dwi" if dwi_ext == ".gz" else dwi_ext[1:]
            dwi_dict[key] = dwi_path
        return dwi_dict

    def _get_fmap(self) -> dict:
        """Title."""
        # Get fmap files
        subj_fmap = os.path.join(self._subj_data, "fmap")
        if not glob.glob(f"{subj_fmap}/sub*"):
            raise FileNotFoundError(f"Expected fmap data at {subj_fmap}")
        _, _ = submit.simp_subproc(f"cp {subj_fmap}/sub* {self._subj_work}")
        fmap_list = sorted(glob.glob(f"{self._subj_work}/sub-*_epi.*"))
        if len(fmap_list) != 4:
            raise FileNotFoundError(f"Expected fmap data at {self._subj_work}")

        fmap_dict = {}
        for fmap_path in fmap_list:
            fmap_ext = os.path.splitext(fmap_path)[1]
            _subj, _sess, dir_val, suff = os.path.basename(fmap_path).split(
                "_"
            )
            _dir = dir_val.split("-")[1]
            key = (
                f"fmap_{_dir}"
                if fmap_ext == ".gz"
                else f"{fmap_ext[1:]}_{_dir}"
            )
            fmap_dict[key] = fmap_path
        return fmap_dict
