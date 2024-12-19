import(mgcv)

#' Supply family names for different scalars
.switch_family <- function(dti_scalar) {
  s_fam <- switch(dti_scalar,
    "dti_fa" = "betar(link = \"logit\")",
    "dti_rd" = "gaussian()",
    "dti_md" = "Gamma(link = \"logit\")",
    "dti_ad" = "gaussian()"
  )
  return(s_fam)
}

#' Supply short tract names
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

#' HGAM of Callosum Superior Parietal.
#'
#' Deprecated, use for testing.
#'
#' Fit HGAMs with AFQ FA values to generate global, group,
#' and ordered group smooths.
#'
#' @param df TODO
export("gam_spar")
gam_spar <- function(df) {
  #
  # fit_S <- bam(
  #   dti_fa ~ s(node_id, bs="tp", k=40, m=2) +
  #     s(subj_id, bs="re") +
  #     s(node_id, scan_name, bs="fs", k=40, m=2),
  #   data = df,
  #   family = betar(link="logit"),
  #   method = "fREML",
  #   discrete = T
  # )
  # gam.check(fit_S, rep=1000)
  # summary(fit_S)
  # plot(fit_S)

  #
  fit_S <- bam(
    dti_fa ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = 40) +
      s(node_id, scan_name, bs = "fs", k = 40),
    data = df,
    family = betar(link = "logit"),
    method = "fREML",
    discrete = T
  )
  # gam.check(fit_S)
  # summary(fit_S)
  # plot(fit_S)

  #
  df$scanOF <- factor(df$scan_name, ordered = T)
  fit_SO <- bam(
    dti_fa ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = 40) +
      s(node_id, by = scanOF, bs = "fs", k = 40),
    data = df,
    family = betar(link = "logit"),
    method = "fREML",
    discrete = T
  )
  # summary(fit_SO)
  # plot(fit_SO)
  return(list(gamGS = fit_S, gamGSO = fit_SO))
}


#' HGAM of Callosum Superior Parietal with random sample
#'
#' Deprecated, use for testing.
#'
export("gam_rand")
gam_rand <- function(df, seed) {
  # Identify subjs with base and post
  subj_list <- unique(df[which(df$scan_name == "post"), ]$subj_id)
  df <- df[which(df$subj_id %in% subj_list), ]

  # Rand subjs
  set.seed(seed)
  subj_list <- sample(subj_list)
  grp_a <- subj_list[1:32]
  grp_b <- subj_list[33:65]

  # Replace dti_fa of post for grp_a with base of grp_b
  idx_a_post <- which(df$scan_name == "post" & df$subj_id %in% grp_a)
  idx_b_base <- which(df$scan_name == "base" & df$subj_id %in% grp_b)
  df[idx_a_post, ]$dti_fa <- df[idx_b_base, ]$dti_fa

  # Run GAM
  fit_rand <- gam_spar(df)
  return(fit_rand)
}


#' Gam interaction for Callosum Superior Parietal and total symptom.
#'
#' Deprecated, used for testing.
#'
export("gam_intx")
gam_intx <- function(df) {
  fit_intx <- bam(
    dti_fa ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = 40) +
      s(tot_symp, by = scan_name, bs = "tp", k = 5) +
      ti(node_id, tot_symp, by = scan_name, bs = c("tp", "tp"), k = c(50, 5)),
    data = df,
    family = betar(link = "logit"),
    method = "fREML",
    discrete = T
  )
  # gam.check(fit_intx)
  # summary(fit_intx)
  # plot(fit_intx)

  df$scanOF <- factor(df$scan_name, ordered = T)
  fit_intxOF <- bam(
    dti_fa ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = 40) +
      s(tot_symp, by = scan_name, bs = "tp", k = 5) +
      ti(node_id, tot_symp, bs = c("tp", "tp"), k = c(50, 5)) +
      ti(
        node_id, tot_symp,
        by = scanOF, bs = c("tp", "tp"), k = c(50, 5), m = 2
      ),
    data = df,
    family = betar(link = "logit"),
    method = "fREML",
    discrete = T
  )

  return(list(gamIntx = fit_intx, gamIntxOF = fit_intxOF))
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


#' Fit HGAM with global, group smooths and wiggliness.
#' @param df TODO
export("gam_gsi")
gam_gsi <- function(df, scalar_name, col_group, k_max = 40) {
  # Validate scalar name
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }

  #
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  names(df)[names(df) == col_group] <- "group"
  df$group <- factor(df$group)

  #
  fit_GSI <- bam(
    dti_scalar ~ s(subj_id, bs = "re") +
      s(node_id, bs = "tp", k = k_max) +
      s(node_id, by = group, bs = "tp", k = k_max),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 4
  )
  return(fit_GSI)
}


#' Fit ordered HGAM with global, group smooths and wiggliness.
#' @param df TODO
export("gam_gsio")
gam_gsio <- function(df, scalar_name, col_group, k_max = 40) {
  # Validate scalar name
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }

  #
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  names(df)[names(df) == col_group] <- "group"
  df$group <- factor(df$group)
  df$groupOF <- factor(df$group, ordered = T)

  #
  fit_GSIO <- bam(
    dti_scalar ~ s(subj_id, bs = "re") +
      s(node_id, bs = "tp", k = k_max) +
      s(node_id, by = groupOF, bs = "tp", k = k_max),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 4
  )
  return(fit_GSIO)
}


#' Fit HGAM with global and interaction with Impact item smooths.
#' @param df TODO
export("gam_g_intx")
gam_g_intx <- function(
    df, scalar_name, impact_meas, ks_max = 40, ki_max = 50) {
  # Validate scalar name
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }

  #
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  names(df)[names(df) == impact_meas] <- "impact_meas"

  #
  fit_G_intx <- bam(
    dti_scalar ~ s(subj_id, bs = "re") +
      s(node_id, bs = "tp", k = ks_max) +
      s(impact_meas, bs = "tp", k = 5) +
      ti(node_id, impact_meas, bs = c("tp", "tp"), k = c(ki_max, 5)),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 4
  )
  return(fit_G_intx)
}


#' Fit HGAM with global, group smooths and wiggliness, and
#' interaction with Impact item smooths.
#' @param df TODO
export("gam_gsi_intx")
gam_gsi_intx <- function(
    df, scalar_name, col_group, impact_meas, ks_max = 40, ki_max = 50) {
  # Validate scalar name
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }

  #
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  names(df)[names(df) == col_group] <- "group"
  df$group <- factor(df$group)
  names(df)[names(df) == impact_meas] <- "impact_meas"

  #
  fit_GSI_intx <- bam(
    dti_scalar ~ s(subj_id, bs = "re") +
      s(node_id, bs = "tp", k = ks_max) +
      s(impact_meas, by = group, bs = "tp", k = 5) +
      ti(
        node_id, impact_meas,
        by = group,
        bs = c("tp", "tp"), k = c(ki_max, 5)
      ),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 4
  )
  return(fit_GSI_intx)
}


#' Fit ordered HGAM with global, group smooths, and
#' interaction with Impact item smooths.
#' @param df TODO
export("gam_gsio_intx")
gam_gsio_intx <- function(
    df, scalar_name, col_group, impact_meas, ks_max = 40, ki_max = 50) {
  # Validate scalar name
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }

  #
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  names(df)[names(df) == col_group] <- "group"
  df$group <- factor(df$group)
  df$groupOF <- factor(df$group, ordered = T)
  names(df)[names(df) == impact_meas] <- "impact_meas"

  #
  fit_GSIO_intx <- bam(
    dti_scalar ~ s(subj_id, bs = "re") +
      s(node_id, bs = "tp", k = ks_max) +
      ti(
        node_id, impact_meas,
        by = groupOF,
        bs = c("tp", "tp"), k = c(ki_max, 5)
      ),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 4
  )
  return(fit_GSIO_intx)
}

#' Fit longitudinal HGAM with global, group smooths.
#' @param df TODO
export("gam_lgsi")
gam_lgsi <- function(df, scalar_name, k_max = 40) {
  # Validate scalar name
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }

  #
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  fit_LGSI <- bam(
    dti_scalar ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = k_max, m = 2) +
      s(node_id, by = scan_name, bs = "tp", k = k_max, m = 1),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 4
  )
  return(fit_LGSI)
}


#' Fit longitudinal HGAM with global, group smooths.
#' @param df TODO
export("gam_lgsio")
gam_lgsio <- function(df, scalar_name, k_max = 40) {
  # Validate scalar name
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }

  #
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  df$scanOF <- factor(df$scan_name, ordered = T)
  fit_LGSIO <- bam(
    dti_scalar ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = k_max, m = 2) +
      s(node_id, by = scanOF, bs = "tp", k = k_max, m = 1),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 4
  )
  return(fit_LGSIO)
}


#' Fit longitudinal HGAM with global, group, and
#' interaction with Impact item smooths.
#'
#' TODO
export("gam_lgsi_intx")
gam_lgsi_intx <- function(df, scalar_name, impact_meas, ks_max = 40, ki_max = 50) {
  # Validate user args
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }
  if (!impact_meas %in%
    c("mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl", "tot_symp")
  ) {
    stop("Unexpected impact_name")
  }

  #
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  names(df)[names(df) == impact_meas] <- "imp_meas"

  #
  fit_LGSI_intx <- bam(
    dti_scalar ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = ks_max) +
      s(imp_meas, by = scan_name, bs = "tp", k = 5) +
      ti(
        node_id, imp_meas,
        by = scan_name,
        bs = c("tp", "tp"), k = c(ki_max, 5)
      ),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 4
  )
  return(fit_LGSI_intx)
}


#' Fit longitudinal HGAM with global, group, and
#' interaction with Impact item smooths.
#'
#' TODO
export("gam_lgsio_intx")
gam_lgsio_intx <- function(df, scalar_name, impact_meas, ks_max = 40, ki_max = 50) {
  # Validate user args
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }
  if (!impact_meas %in%
    c("mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl", "tot_symp")
  ) {
    stop("Unexpected impact_name")
  }

  #
  fam_scalar <- .switch_family(scalar_name)
  names(df)[names(df) == scalar_name] <- "dti_scalar"
  names(df)[names(df) == impact_meas] <- "imp_meas"
  df$scanOF <- factor(df$scan_name, ordered = T)

  #
  fit_LGSIO_intx <- bam(
    dti_scalar ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = ks_max) +
      s(imp_meas, by = scan_name, bs = "tp", k = 5) +
      ti(node_id, imp_meas, bs = c("tp", "tp"), k = c(ki_max, 5)) +
      ti(
        node_id, imp_meas,
        by = scanOF,
        bs = c("tp", "tp"), k = c(ki_max, 5), m = 2
      ),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T
  )
  return(fit_LGSIO_intx)
}
