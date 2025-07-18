# Methods for executing GAM models.
#
# Functions are named loosely referencing the Pedersen 2019 paper,
# where g = global, i = group (without shared wiggliness). Additions
# involve d = delta/difference data, o = ordered factors, and
# l = longitudinal. Additional parts of function name help describe
# the data or analysis, e.g. intx = tensor product interaction
# smooths are included.

import(mgcv)


#' Supply family names for different scalars
#'
#' @param dti_scalar Name of scalar from AFQ csv.
#' @returns Family and link function for gam/bam models.
.switch_family <- function(dti_scalar) {
  s_fam <- switch(dti_scalar,
    "dti_fa" = "betar(link = \"logit\")",
    "dti_rd" = "gaussian()",
    "dti_md" = "Gamma(link = \"logit\")",
    "dti_ad" = "gaussian()",
    "delta" = "gaussian()"
  )
  return(s_fam)
}


#' Fit HGAM for all tract FA differences.
#'
#' Test for differences between times 1 and 2 of modeling data. Similar
#' to mod_ldi, but without the extra scan_name factor. Used for investigating
#' effect of rerunning tractography on baseline data.
#'
#' @param df Dataframe of AFQ with columns subj_id, tract_name, node_id, delta.
#' @returns mgcv::bam fit object.
export("mod_di")
mod_di <- function(df) {
  fam_scalar <- .switch_family("delta")
  fit_DI <- bam(
    delta ~ s(subj_id, by = tract_name, bs = "re") +
      s(node_id, by = tract_name, bs = "tp", k = 15) +
      tract_name,
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 12
  )
  return(fit_DI)
}


#' Fit a RTP-Post x Time GAM.
#'
#' Model change in FA fro Post to RTP the tensor product interaction
#' smooth of time (days between scans) to potentially detect FA
#' recovery or worsening.
#'
#' @param df Dataframe of AFQ data for single tract with delta column.
#' @param ks_max Optional, max k-value for group smooths.
#' @param ki_max Optional, mak k-value for group interaction smooths.
#' @returns mgcv::bam fit object.
export("mod_di_time")
mod_di_time <- function(df, ks_max = 15, ki_max = 20) {
  fam_scalar <- .switch_family("delta")
  fit_DI_time <- bam(
    delta.rtp_post ~ s(subj_id, bs = "re") +
      s(node_id, bs = "tp", k = ks_max, m = 2) +
      s(days.rtp_post, bs = "tp", k = 5) +
      ti(
        node_id, days.rtp_post,
        bs = c("tp", "tp"), k = c(ki_max, 5), m = 1
      ),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 12
  )
  # gam.check(fit_DI_time)
  # plot(fit_DI_time)
  return(fit_DI_time)
}


#' Model ImPACT composites as a function of visit (1-3).
#'
#' While visit is not continuous but categorical, this will show
#' change in measures that result from concussion and/or RTP.
#'
#' @param df Dataframe (tidy) of ImPACT values.
#' @param beh Column name of behavior (e.g. mem_vis).
#' @param fit_meth Optional, fitting method for semi-parametric data (prop,
#' log, gamma, or negbin).
#' @param adj_value Optional, adjust values by N (e.g. 0.1 or -0.5). Used
#' to help resolve zero- or one-point inflation.
#' @returns mgcv::bam fit object.
export("mod_imp")
mod_imp <- function(df, beh, fit_meth = "None", adj_value = FALSE) {
  if (fit_meth == "prop") {
    df$beh_tx <- df[, beh] / 100
    link_fam <- "betar(link = \"logit\")"
    if (adj_value) {
      df$beh_tx <- df$beh_tx + adj_value
    }
  }

  if (fit_meth == "log") {
    df$beh_tx <- log(df[, beh])
    link_fam <- "gaussian()"
  }

  if (fit_meth == "gamma") {
    df$beh_tx <- df[, beh]
    link_fam <- "Gamma(link = \"log\")"
    if (adj_value) {
      df$beh_tx <- df$beh_tx + adj_value
    }
  }

  if (fit_meth == "negbin") {
    df$beh_tx <- df[, beh]
    link_fam <- "nb()"
  }

  if (fit_meth == "None") {
    df$beh_tx <- df[, beh]
    link_fam <- "gaussian()"
  }

  fit_beh <- bam(
    beh_tx ~ s(scan_count, bs = "tp", k = 3) +
      s(subj_id, bs = "re"),
    data = df,
    family = link_fam,
    method = "fREML",
    discrete = T
  )
  return(fit_beh)
}


#' Fit longitudinal HGAM for all tracts of FA differences.
#'
#' Manually calculate the interaction of factors tract_name and comp_scan,
#' then model all tract differences with group wiggliness. No global smooth
#' is used, so this is similar to the Pedersen 2019 model 'I'. This
#' effectively pools subject variance across both tract_name and scan_name,
#' while simplifying the nested factors. Differences are used to reduce the
#' requested number of smooths, reduce runtime, and maintain sufficient dof.
#'
#' @param df Dataframe of AFQ with comp_scan and delta columns (output by
#'  misc_help$calc_fa_delta).
#' @returns mgcv::bam fit object.
export("mod_ldi")
mod_ldi <- function(df) {
  df$tract_scan <- interaction(df$tract_name, df$comp_scan)
  fam_scalar <- .switch_family("delta")
  fit_LDI <- bam(
    delta ~ s(subj_id, by = tract_scan, bs = "re") +
      s(node_id, by = tract_scan, bs = "tp", k = 15) +
      tract_name + comp_scan + tract_scan,
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 12
  )
  return(fit_LDI)
}


#' Fit longitudinal HGAM for all tracts of FA differences for one subject.
#'
#' Very similar to mod_ldi, but used to model data from single subject
#' scanned multiple times (so no random effect of subject).
#'
#' @param df Dataframe of AFQ with comp_scan and delta columns (output by
#'  misc_help$calc_fa_delta).
#' @returns mgcv::bam fit object.
export("mod_ldi_rescan")
mod_ldi_rescan <- function(df) {
  df$tract_scan <- interaction(df$tract_name, df$comp_scan)
  fam_scalar <- .switch_family("delta")
  fit_LDI <- bam(
    delta ~ s(node_id, by = tract_scan, bs = "tp", k = 15) +
      tract_name + comp_scan + tract_scan,
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 12
  )
  return(fit_LDI)
}


#' Fit longitudinal HGAM with global, group smooths and wiggliness.
#'
#' Pool within subject across multiple scans. Models scalar name of a single
#' tract across multiple time points.
#'
#' @param df Dataframe of AFQ data for single tract.
#' @param scalar_name DWI metric, dti_fa, dti_rd, dti_md, or dti_ad.
#' @returns mgcv::bam fit object.
export("mod_lgi")
mod_lgi <- function(df, scalar_name, k_max = 15) {
  # Validate scalar name
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }

  # Determine family and relevant cols
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"

  # Fit data
  fit_LGI <- bam(
    dti_scalar ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = k_max, m = 2) +
      s(node_id, by = scan_name, bs = "tp", k = k_max, m = 1),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 4
  )
  return(fit_LGI)
}


#' Fit longitudinal HGAM with global, group smooths and wiggliness,
#' and scalar-impact interaction smooths.
#'
#' Use a tensor product interaction smooth to see if impact and scalar
#' values are related.
#'
#' @param df Dataframe of AFQ data for single tract.
#' @param impact_meas IMPACT metric: mem_ver, mem_vis, vis_mot, rx_time,
#'  imp_ctl, or tot_symp.
#' @param scalar_name Optional, DWI metric: dti_fa, dti_rd, dti_md, or dti_ad.
#' @param ks_max Optional, max k-value for group smooths.
#' @param ki_max Optional, mak k-value for group interaction smooths.
#' @returns mgcv::bam fit object.
export("mod_lgi_intx")
mod_lgi_intx <- function(
    df, impact_meas, scalar_name = "dti_fa", ks_max = 15, ki_max = 20) {
  # Validate user args
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }
  if (!impact_meas %in%
    c("mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl", "tot_symp")
  ) {
    stop("Unexpected impact_name")
  }

  # Find family, set column  names, and make ordered factor
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  names(df)[names(df) == impact_meas] <- "imp_meas"

  # Fit and return model
  fit_LGI_intx <- bam(
    dti_scalar ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = ks_max, m = 2) +
      s(imp_meas, by = scan_name, bs = "tp", k = 5) +
      ti(
        node_id, imp_meas,
        by = scan_name,
        bs = c("tp", "tp"), k = c(ki_max, 5), m = 1
      ),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 12
  )
  return(fit_LGI_intx)
}


#' Fit longitudinal HGAM with global, group (ordered) smooths and wiggliness.
#'
#' Pool within subject across multiple scans, and compare group B (or more)
#' smooth to that of group A using ordered factors.
#'
#' @param df Dataframe of AFQ data for single tract.
#' @param scalar_name DWI metric, dti_fa, dti_rd, dti_md, or dti_ad.
#' @returns mgcv::bam fit object.
export("mod_lgio")
mod_lgio <- function(df, scalar_name, k_max = 15) {
  # Validate scalar name
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }

  # Determine family and relevant cols
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  df$scanOF <- factor(df$scan_name, ordered = T)

  # Fit data
  fit_LGIO <- bam(
    dti_scalar ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = k_max, m = 2) +
      s(node_id, by = scanOF, bs = "tp", k = k_max, m = 1),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 4
  )
  return(fit_LGIO)
}


#' Fit longitudinal HGAM with global, group (ordered) smooths and wiggliness,
#' and scalar-Impact interaction smooths.
#'
#' Use a tensor product interaction smooth to see if impact and scalar
#' values are related, using group as ordered factors to compare against
#' baseline.
#'
#' @param df Dataframe of AFQ data for single tract.
#' @param impact_meas IMPACT metric: mem_ver, mem_vis, vis_mot, rx_time,
#'  imp_ctl, or tot_symp.
#' @param scalar_name Optional, DWI metric: dti_fa, dti_rd, dti_md, or dti_ad.
#' @param ks_max Optional, max k-value for group smooths.
#' @param ki_max Optional, mak k-value for group interaction smooths.
#' @returns mgcv::bam fit object.
export("mod_lgio_intx")
mod_lgio_intx <- function(
    df, impact_meas, scalar_name = "dti_fa", ks_max = 15, ki_max = 20) {
  # Validate user args
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }
  if (!impact_meas %in%
    c("mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl", "tot_symp")
  ) {
    stop("Unexpected impact_name")
  }

  # Find family, set column  names, and make ordered factor
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  names(df)[names(df) == impact_meas] <- "imp_meas"
  df$scanOF <- factor(df$scan_name, ordered = T)

  # Fit and return model
  fit_LGIO_intx <- bam(
    dti_scalar ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = ks_max, m = 2) +
      s(imp_meas, by = scan_name, bs = "tp", k = 5) +
      ti(node_id, imp_meas, bs = c("tp", "tp"), k = c(ki_max, 5), m = 1) +
      ti(
        node_id, imp_meas,
        by = scanOF,
        bs = c("tp", "tp"), k = c(ki_max, 5), m = 1
      ),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 12
  )
  return(fit_LGIO_intx)
}


#' Fit longitudinal HGAM for all tracts of dti_fa values.
#'
#' Deprecated.
#'
#' Similar to mod_ldi, but without the delta calculation.
#'
#' @param df Dataframe of AFQ with columns subj_id, tract_name, scan_name,
#'  node_id, dti_fa.
#' @returns mgcv::bam fit object.
export("mod_li")
mod_li <- function(df) {
  df$tract_scan <- interaction(df$tract_name, df$scan_name)
  fam_scalar <- .switch_family("dti_fa")
  fit_I <- bam(
    dti_fa ~ s(subj_id, by = tract_scan, bs = "re") +
      s(node_id, by = tract_scan, bs = "tp", k = 15) +
      tract_name + scan_name + tract_scan,
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 12
  )
  return(fit_I)
}


#' Supply short tract names.
#'
#' @param tract_name Name of tract from AFQ csv.
#' @returns Abbreviated tract name.
export("switch_tract")
switch_tract <- function(tract_name) {
  s_name <- switch(as.character(tract_name),
    "Left Anterior Thalamic" = "laThal",
    "Left Cingulum Cingulate" = "lCCing",
    "Left Corticospinal" = "lCS",
    "Left Inferior Fronto-occipital" = "lIFO",
    "Left Inferior Longitudinal" = "lIL",
    "Left Superior Longitudinal" = "lSL",
    "Left Arcuate" = "lArc",
    "Left Uncinate" = "lUnc",
    "Left Posterior Arcuate" = "lpArc",
    "Left Vertical Occipital" = "lvOcc",
    "Callosum Anterior Frontal" = "CCaf",
    "Callosum Occipital" = "CCocc",
    "Callosum Orbital" = "CCorb",
    "Callosum Posterior Parietal" = "CCpp",
    "Callosum Superior Parietal" = "CCsp",
    "Right Anterior Thalamic" = "raThal",
    "Right Cingulum Cingulate" = "rCCing",
    "Right Corticospinal" = "rCS",
    "Right Inferior Fronto-occipital" = "rIFO",
    "Right Inferior Longitudinal" = "rIL",
    "Right Superior Longitudinal" = "rSL",
    "Right Arcuate" = "rArc",
    "Right Uncinate" = "rUnc",
    "Right Posterior Arcuate" = "rpArc",
    "Right Vertical Occipital" = "rvOcc",
    "Callosum Motor" = "CCmot",
    "Callosum Superior Frontal" = "CCsf",
    "Callosum Temporal" = "CCtemp",
  )
  return(s_name)
}


#' Write GAM summary stats to txt file.
#'
#' @param gam_obj GAM object returned by mgcv.
#' @param out_file String path to output file.
export("write_gam_stats")
write_gam_stats <- function(gam_obj, out_file) {
  utils::capture.output(
    summary(gam_obj),
    file = out_file
  )
}
