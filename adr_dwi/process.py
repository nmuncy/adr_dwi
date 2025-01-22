"""Methods for processing data.

cnt_nvol: Determine number of EPI volumes.
BidsAdr: Methods for BIDSifying anat, dwi, and fmap shared ADR rawdata.
DwiPreproc: Methods for preprocessing DWI data via FSL's topup and eddy.

"""

import os
import glob
import json
import shutil
import zipfile
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


class _BidsAdrAnat(_SetSubjSess):
    """BIDSify ADR anat directory.

    Assumes local attic organization of anat containing MEMPRAGE files and
    SWI files removed.

    Example:
        bids_anat = process._BidsAdrAnat()
        bids_anat.subj = "sub-0001"
        bids_anat.sess = "ses-1"
        bids_anat.data_dir = "/path/to/BIDS/dir"
        bids_anat.fix_anat_names()

    """

    def __init__(self):
        """Initialize _BidsAdrAnat."""
        log.write.info("Initializing _BidsAdrAnat")
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


class _BidsAdrDwi(_SetSubjSess):
    """BIDSify ADR dwi directory.

    Assumes local attic organization of DWI with b0 and
    b1000t files removed.

    Example:
        bids_dwi = process._BidsAdrDwi()
        bids_dwi.subj = "sub-0001"
        bids_dwi.sess = "ses-1"
        bids_dwi.data_dir = "/path/to/BIDS/dir"
        bids_dwi.fix_dwi_names()

    """

    def __init__(self):
        """Initialize _BidsAdrDwi."""
        log.write.info("Initializing _BidsAdrDwi")
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


class _BidsAdrFmap(_SetSubjSess):
    """BIDSify ADR fmap directory.

    Assumes local attic organization of fmap files.

    Notes:
        - Order of method execution is important.
        - Requires BIDSified DWI files.

    Example:
        bids_fmap = process._BidsAdrFmap()
        bids_fmap.subj = "sub-0001"
        bids_fmap.sess = "ses-1"
        bids_fmap.data_dir = "/path/to/BIDS/dir"
        bids_fmap.fix_fmap_names()
        bids_fmap.fix_fmap_intended()
        bids_fmap.fix_fmap_vols()

    """

    def __init__(self):
        """Initialize _BidsAdrFmap."""
        log.write.info("Initializing _BidsAdrFmap")
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


class BidsAdr(_BidsAdrAnat, _BidsAdrDwi, _BidsAdrFmap):
    """Provide methods for BIDSifying ADR anat, dwi, and famp data.

    Combine BIDS classes into one for centralized methods, inherits
    _BidsAdrAnat, _BidsAdrDwi, and _BidsAdrFmap.

    Example:
        # Initialize and set attrs
        bids_org = process.BidsAdr()
        bids_org.subj = "sub-0001"
        bids_org.sess = "ses-1"
        bids_org.data_dir = "/path/to/BIDS/dir"

        # Execute fixing methods
        bids_org.fix_anat_names()
        bids_org.fix_dwi_names()
        bids_org.fix_fmap_names()
        bids_org.fix_fmap_intended()
        bids_org.fix_fmap_vols()

    """

    def __init__(self):
        """Initialize BidsAdr."""
        log.write.info("Initializing BidsAdr")
        _BidsAdrAnat.__init__(self)
        _BidsAdrDwi.__init__(self)
        _BidsAdrFmap.__init__(self)


class _BidsHcpAnat:
    """Title."""

    def __init__(self):
        """Initialize _BidsHcpAnat."""
        log.write.info("Initializing _BidsHcpAnat")

    def _set_subj(self):
        """Title."""
        self._subj_id = os.path.dirname(self._zip_path).split("_")[0]
        self._subj = f"sub-{self._subj_id}"

    def _unzip_struct(self):
        """Title."""
        log.write.info(f"Unzipping: {self._zip_path}")

        #
        self._set_subj()
        file_dir = os.path.dirname(self._zip_path)
        self._unzip_dir = os.path.join(file_dir, self._subj_id)
        if os.path.exists(self._unzip_dir):
            return

        #
        with zipfile.ZipFile(self._zip_path, "r") as zf:
            zf.extractall(file_dir)
        chk_file = os.path.join(
            self._unzip_dir, "unprocessed", "3T", f"{self._subj_id}_3T.csv"
        )
        if not os.path.exists(chk_file):
            raise FileNotFoundError(chk_file)

    def bids_anat(self, zip_path: PT):
        """Title."""
        self._zip_path = zip_path
        self._unzip_struct()

        #
        search_dir = os.path.join(self._unzip_dir, "unprocessed", "3T")
        t1_list = sorted(
            glob.glob(f"{search_dir}/T1*/{self._subj_id}_3T_T1w_*.nii.gz")
        )
        if not t1_list:
            raise FileNotFoundError(
                f"Expected unzipped T1w files in: {search_dir}"
            )
        t1_orig = t1_list[0]


class BidsHcp(_BidsHcpAnat):
    """Title."""

    def __init__(self):
        """Initialize BidsHcp."""
        log.write.info("Initializing BidsHcp")
        _BidsHcpAnat.__init__(self)


class _FslTopup:
    """Methods for preparing for, executing FSL's topup."""

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

        # Build FSL command and submit to subprocess.
        fsl_cmd = ["fslroi", in_path, os.path.join(out_dir, out_name), "0 1"]
        out, err = submit.simp_subproc(" ".join(fsl_cmd))
        if not os.path.exists(out_path):
            raise FileNotFoundError(out_path)
        return out_path

    def combine_b0(
        self, ap_path: PT, pa_path: PT, out_name: str = "tmp_AP_PA_b0"
    ) -> PT:
        """Combine AP and PA b0 files via fslmerge.

        Args:
            ap_path: Location of AP b0 file, see _FslTopup.extract_b0().
            pa_path: Location of PA b0 file, same note.
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

        # Build FSL command and submit to subprocess.
        fsl_cmd = [
            "fslmerge",
            f"-t {os.path.join(out_dir, out_name)}",
            ap_path,
            pa_path,
        ]
        out, err = submit.simp_subproc(" ".join(fsl_cmd))
        if not os.path.exists(out_path):
            raise FileNotFoundError(out_path)
        return out_path

    def acq_param(
        self, json_path: PT, out_name: str = "tmp_acq_param.txt"
    ) -> PT:
        """Write an acq_param file for eddy.

        Args:
            json_path: Location of JSON sidecar.
            out_name: Optional, acq param file name.

        Returns:
            Location of output file (same dir as json_path).

        Raises:
            ValueError: Failed to write info to output file.

        """
        log.write.info("Writing acq_param.txt")
        out_dir = os.path.dirname(json_path)
        out_path = os.path.join(out_dir, out_name)
        if os.path.exists(out_path) and not helper.is_empty(out_path):
            return out_path

        # Use TotalReadoutTime field from JSON sidecar
        # for building param file.
        with open(json_path) as jf:
            json_dict = json.load(jf)
        line_a = f"0 -1 0 {json_dict['TotalReadoutTime']}\n"
        line_b = f"0 1 0 {json_dict['TotalReadoutTime']}\n"
        with open(out_path, "w") as f:
            f.write(line_a)
            f.write(line_b)

        if helper.is_empty(out_path):
            raise ValueError(f"Empty file: {out_path}")
        return out_path

    def run_topup(
        self,
        ap_pa_b0: PT,
        acq_param: PT,
        job_name: str,
        log_dir: PT,
        out_name: str = "tmp_topup",
    ) -> tuple:
        """Calculate distortion correction via topup.

        Args:
            ap_pa_b0: Location of combined AP/PA b0 file, see
                _FslTopup.combine_b0().
            acq_param: Location of acquisition parameter file,
                see _FslTopup.acq_param().
            job_name: Name of job for scheduler.
            log_dir: Location for capturing STDOUT/ERR.
            out_name: Optional, output prefix.

        Returns:
            tuple:
                [0] = Location of fieldcoef file.
                [1] = Location of unwarped file.

        Raises:
            FileNotFoundError: Missing output files.

        """
        log.write.info("Running topup")
        out_dir = os.path.dirname(ap_pa_b0)
        out_coef = os.path.join(out_dir, f"{out_name}_fieldcoef.nii.gz")
        out_unwarp = os.path.join(out_dir, "tmp_b0_unwarped.nii.gz")
        if os.path.exists(out_coef) and os.path.exists(out_unwarp):
            return (out_coef, out_unwarp)

        # Build FSL command and submit to scheduler.
        fsl_cmd = [
            "topup",
            f"--imain={ap_pa_b0}",
            f"--datain={acq_param}",
            "--config=b02b0.cnf",
            f"--out={os.path.join(out_dir, out_name)}",
            f"--iout={os.path.join(out_dir, 'tmp_b0_unwarped')}",
        ]
        out, err = submit.sched_subproc(" ".join(fsl_cmd), job_name, log_dir)
        for chk_out in [out_coef, out_unwarp]:
            if not os.path.exists(chk_out):
                raise FileNotFoundError(chk_out)
        return (out_coef, out_unwarp)


class _FslEddy:
    """Methods for preparing for, executing FSL's eddy."""

    def get_mean(self, file_path: PT) -> PT:
        """Extract temporal mean via fslmaths.

        Args:
            file_path: Location of input DWI file.

        Returns:
            Location of output mean file (same dir as file_path).

        Raises:
            FileNotFoundError: Missing output file.

        """
        log.write.info("Running fslmaths")
        out_path = file_path.replace(".nii.gz", "_mean.nii.gz")
        if os.path.exists(out_path):
            return out_path

        # Build FSL command and submit to subprocess.
        fsl_cmd = ["fslmaths", file_path, f"-Tmean {out_path}"]
        _, _ = submit.simp_subproc(" ".join(fsl_cmd))
        if not os.path.exists(out_path):
            raise FileNotFoundError(out_path)
        return out_path

    def brain_mask(
        self,
        file_path: PT,
        out_name: str = "tmp_brain.nii.gz",
        f_val: float = 0.2,
    ) -> PT:
        """Make a brain mask via FSL's bet with f=0.2.

        Args:
            file_path: Location of input DWI file.
            out_name: Optional, output file name.
            f_val: Optional, fractional intensity threshold.

        Returns:
            Location of output brain mask (same dir as file_path).

        Raises:
            FileNotFoundError: Missing output file.

        """
        log.write.info("Running bet")
        out_dir = os.path.dirname(file_path)
        out_path = os.path.join(out_dir, out_name)
        if os.path.exists(out_path):
            return out_path

        # Use mean volume as input
        fsl_mean = self.get_mean(file_path)
        fsl_cmd = [
            "bet",
            fsl_mean,
            out_path,
            "-m",
            f"-f {f_val}",
        ]
        _, _ = submit.simp_subproc(" ".join(fsl_cmd))
        if not os.path.exists(out_path):
            raise FileNotFoundError(out_path)
        return out_path

    def _num_vol(self, file_path: PT) -> int:
        """Return number of volumes in file."""
        log.write.info("Finding number of vols")
        fsl_cmd = [
            "fslinfo",
            file_path,
            "| grep -w dim4",
            "| awk '{print $2}'",
        ]
        out, _err = submit.simp_subproc(" ".join(fsl_cmd))
        return int(out.decode("utf-8"))

    def write_index(
        self, file_path: PT, out_name: str = "tmp_index.txt"
    ) -> PT:
        """Build an index file from number of DWI volumes.

        Args:
            file_path: Location of input DWI file.
            out_name: Optional, output file name.

        Returns:
            Location of index file (same dir as file_path).

        Raises:
            ValueError: Failed to write data to index file.

        """
        log.write.info("Writing index")
        out_path = os.path.join(os.path.dirname(file_path), out_name)
        if os.path.exists(out_path) and not helper.is_empty(out_path):
            return out_path

        # Write a line to index for each volume
        num_vol = self._num_vol(file_path)
        cnt = 0
        with open(out_path, "w") as f:
            while cnt < num_vol:
                f.write("1\n")
                cnt += 1

        if helper.is_empty(out_path):
            raise ValueError(f"Empty file: {out_path}")
        return out_path

    def _split_ext(self, in_str: str) -> str:
        """Return file name without extension."""
        return in_str.split(".")[0]

    def run_eddy(
        self,
        dwi_data: PT,
        dwi_bvec: PT,
        dwi_bval: PT,
        dwi_json: PT,
        dwi_topup: PT,
        brain_mask: PT,
        index: PT,
        acq_param: PT,
        out_name: str,
        job_name: str,
        log_dir: PT,
        mporder: int = 15,
    ) -> PT:
        """Preprocess DWI data via FSL's eddy.

        Args:
            dwi_data: Location of raw DWI data.
            dwi_bvec: Location of bvec file.
            dwi_bval: Location of bval file.
            dwi_json: Location of json file.
            dwi_topup: Location of topup output, see _FslTopup.run_topup().
            brain_mask: Location of brain mask, see FslEddy.brain_mask().
            index: Location of index file, see FslEddy.write_index().
            acq_param: Location of acquisition parameter file, see
                _FslTopup.acq_param().
            out_name: Output file name.
            job_name: Name of job for scheduler.
            log_dir: Location for capturing STDOUT/ERR.
            mporder: Optional, number of basis functions, typically
                number of slices / 4.

        Returns:
            Location of ouput file (same dir as dwi_data).

        Raises:
            FileNotFoundError: Missing output file.

        """
        log.write.info("Running eddy_openmp")
        out_dir = os.path.dirname(dwi_data)
        out_path = os.path.join(out_dir, out_name)
        if os.path.exists(out_path):
            return out_path

        # Manage extension/suffix reqs
        in_main = os.path.basename(self._split_ext(dwi_data))
        mask = os.path.basename(self._split_ext(brain_mask))
        bvecs = os.path.basename(dwi_bvec)
        bvals = os.path.basename(dwi_bval)
        json = os.path.basename(dwi_json)
        out = os.path.basename(self._split_ext(out_path))
        idx = os.path.basename(index)
        acqp = os.path.basename(acq_param)
        topup = os.path.basename(dwi_topup).split("_field")[0]

        # Build eddy command and submit to scheduler
        fsl_cmd = [
            f"cd {out_dir};",
            "eddy",
            "-v",
            f"--imain={in_main}",
            f"--mask={mask}",
            f"--index={idx}",
            f"--acqp={acqp}",
            f"--bvecs={bvecs}",
            f"--bvals={bvals}",
            f"--json={json}",
            f"--topup={topup}",
            f"--out={out}",
            f"--mporder={mporder}",
            "--repol",
            "--estimate_move_by_susceptibility",
        ]
        out, err = submit.sched_gpu(
            " ".join(fsl_cmd),
            job_name,
            log_dir,
            num_hours=4,
            mem_gig=12,
        )
        if not os.path.exists(out_path):
            raise FileNotFoundError(out_path)
        return out_path


class DwiPreproc(_FslTopup, _FslEddy):
    """Methods for preprocessing DWI with FSL tools.

    Inhertis _FslTopup, _FslEddy.

    Requires:
        FSL to be executable in system OS.

    Args:
        subj: BIDS subject ID.
        sess: BIDS session ID.
        work_dir: Location for intermediates.
        data_dir: Location of BIDS organized directory.

    Raises:
        EnvironmentError: FSL (fslroi) not executable in environment.

    """

    def __init__(self, subj: str, sess: str, work_dir: PT, data_dir: PT):
        """Initialize FslPreproc."""
        log.write.info("Initializing FslPreproc")

        # Validate environment
        if not shutil.which("fslroi"):
            raise EnvironmentError("Missing FSL in environment")

        self._subj = subj
        self._sess = sess
        self._work_dir = work_dir
        self._data_dir = data_dir
        self._work_dir = work_dir

    def setup(self) -> tuple:
        """Set attrs and copy data from data to work dirs.

        Returns:
            tuple:
                [0] = dict of format {
                    "dwi": PT, "bval": PT, "bvec": PT, "json": PT
                }
                [1] = dict of format {
                    "fmap_AP": PT,
                    "fmap_PA": PT,
                    "json_AP": PT,
                    "json_PA": PT
                }

        """
        log.write.info("Running setup")

        # Set attrs
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

        # Make dirs
        for chk_path in [self._subj_work, self._subj_deriv]:
            if not os.path.exists(chk_path):
                os.makedirs(chk_path)

        # Coordinate data copy
        dwi_dict = self._get_dwi()
        fmap_dict = self._get_fmap()
        return (dwi_dict, fmap_dict)

    def _get_dwi(self) -> dict:
        """Copy DWI files to work.

        Returns:
            dict: {"dwi": PT, "bval": PT, "bvec": PT, "json": PT}

        Raises:
            FileNotFoundError: Missing data in data or work dirs.
            ValueError: Number of work DWI files != 4.

        """

        # Get raw DWI files
        subj_dwi = os.path.join(self._subj_data, "dwi")
        if not glob.glob(f"{subj_dwi}/sub*"):
            raise FileNotFoundError(f"Expected dwi data at {subj_dwi}")
        _, _ = submit.simp_subproc(f"cp {subj_dwi}/sub* {self._subj_work}")
        dwi_list = sorted(glob.glob(f"{self._subj_work}/sub-*_dir-AP_dwi.*"))
        if len(dwi_list) != 4:
            raise ValueError(
                f"Missing/unexpected dwi data at {self._subj_work}"
            )

        # Build output dict
        dwi_dict = {}
        for dwi_path in dwi_list:
            dwi_ext = os.path.splitext(dwi_path)[1]
            key = "dwi" if dwi_ext == ".gz" else dwi_ext[1:]
            dwi_dict[key] = dwi_path
        return dwi_dict

    def _get_fmap(self) -> dict:
        """Copy fmap files to work.

        Returns:
            dict: {
                "fmap_AP": PT,
                "fmap_PA": PT,
                "json_AP": PT,
                "json_PA": PT
            }

        Raises:
            FileNotFoundError: Missing data in data or work dirs.
            ValueError: Number of work fmap files != 4.

        """
        # Get fmap files
        subj_fmap = os.path.join(self._subj_data, "fmap")
        if not glob.glob(f"{subj_fmap}/sub*"):
            raise FileNotFoundError(f"Expected fmap data at {subj_fmap}")
        _, _ = submit.simp_subproc(f"cp {subj_fmap}/sub* {self._subj_work}")
        fmap_list = sorted(glob.glob(f"{self._subj_work}/sub-*_epi.*"))
        if len(fmap_list) != 4:
            raise ValueError(
                f"Missing/unexpected fmap data at {self._subj_work}"
            )

        # Build output dict
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
