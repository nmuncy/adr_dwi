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
#' TODO remove
#'
#' Fit HGAMs with AFQ FA values to generate global, group,
#' and ordered group smooths.
#'
#' @param df TODO
.gam_spar <- function(df) {
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
#' TODO remove
#'
.gam_rand <- function(df, seed) {
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
#' TODO remove
#'
.gam_intx <- function(df) {
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
#' @param re_test Optional, use re.test arg of summary.
export("write_gam_stats")
write_gam_stats <- function(gam_obj, out_file) {
  utils::capture.output(
    summary(gam_obj),
    file = out_file
  )
}


#' #' Fit dwi data via HGAM with global, group smooths and wiggliness.
#' #' 
#' #' @param df Dataframe of AFQ data for single tract.
#' #' @param scalar_name DWI metric, dti_fa, dti_rd, dti_md, or dti_ad.
#' #' @param col_group Column of df holding group factor.
#' #' @param k_max Numeric, parameter for smooth k argument.
#' #' @returns mgcv::bam fit object.
#' export("mod_gi")
#' mod_gi <- function(df, scalar_name, col_group, k_max = 40) {
#'   # Validate scalar name
#'   if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
#'     stop("Unexpected scalar_name")
#'   }
#' 
#'   # Identify columns of df for scalar and group, as well
#'   # as family used for scalar.
#'   fam_scalar <- .switch_family(scalar_name)
#'   names(df)[names(df) == scalar_name] <- "dti_scalar"
#'   names(df)[names(df) == col_group] <- "group"
#'   df$group <- factor(df$group)
#' 
#'   # Fit data
#'   fit_GI <- bam(
#'     dti_scalar ~ s(subj_id, bs = "re") +
#'       s(node_id, bs = "tp", k = k_max, m = 2) +
#'       s(node_id, by = group, bs = "tp", k = k_max), m = 1,
#'     data = df,
#'     family = fam_scalar,
#'     method = "fREML",
#'     discrete = T,
#'     nthreads = 4
#'   )
#'   return(fit_GI)
#' }
#' 
#' 
#' #' Fit dwi data via ordered HGAM with global, group smooths and wiggliness.
#' #' 
#' #' Compare group B smooth to that of group A using ordered factors.
#' #' 
#' #' @param df Dataframe of AFQ data for single tract.
#' #' @param scalar_name DWI metric, dti_fa, dti_rd, dti_md, or dti_ad.
#' #' @param col_group Column of df holding group factor.
#' #' @param k_max Numeric, parameter for smooth k argument.
#' #' @returns mgcv::bam fit object.
#' export("mod_gio")
#' mod_gio <- function(df, scalar_name, col_group, k_max = 40) {
#'   # Validate scalar name
#'   if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
#'     stop("Unexpected scalar_name")
#'   }
#' 
#'   # Setup, make ordered factor column
#'   fam_scalar <- .switch_family(scalar_name)
#'   names(df)[names(df) == scalar_name] <- "dti_scalar"
#'   names(df)[names(df) == col_group] <- "group"
#'   df$group <- factor(df$group)
#'   df$groupOF <- factor(df$group, ordered = T)
#' 
#'   # Fit data
#'   fit_GIO <- bam(
#'     dti_scalar ~ s(subj_id, bs = "re") +
#'       s(node_id, bs = "tp", k = k_max, m = 2) +
#'       s(node_id, by = groupOF, bs = "tp", k = k_max, m = 1),
#'     data = df,
#'     family = fam_scalar,
#'     method = "fREML",
#'     discrete = T,
#'     nthreads = 4
#'   )
#'   return(fit_GIO)
#' }
#' 
#' 
#' #' Fit dwi data via GAM with global, impact, and interaction smooths. 
#' #' 
#' #' @param df Dataframe of AFQ data for single tract.
#' #' @param scalar_name DWI metric, dti_fa, dti_rd, dti_md, or dti_ad.
#' #' @param impact_meas Column name of impact measure.
#' #' @param ks_max Numeric, parameter for global smooth k argument.
#' #' @param ki_max Numeric, parameter for interaction smooth k argument.
#' #' @returns mgcv::bam fit object.
#' export("mod_g_intx")
#' mod_g_intx <- function(
#'     df, scalar_name, impact_meas, ks_max = 40, ki_max = 50) {
#'   # Validate scalar name
#'   if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
#'     stop("Unexpected scalar_name")
#'   }
#' 
#'   # Identify family and columns
#'   fam_scalar <- .switch_family(scalar_name)
#'   names(df)[names(df) == scalar_name] <- "dti_scalar"
#'   names(df)[names(df) == impact_meas] <- "impact_meas"
#' 
#'   # Fit data
#'   fit_G_intx <- bam(
#'     dti_scalar ~ s(subj_id, bs = "re") +
#'       s(node_id, bs = "tp", k = ks_max) +
#'       s(impact_meas, bs = "tp", k = 5) +
#'       ti(node_id, impact_meas, bs = c("tp", "tp"), k = c(ki_max, 5)),
#'     data = df,
#'     family = fam_scalar,
#'     method = "fREML",
#'     discrete = T,
#'     nthreads = 4
#'   )
#'   return(fit_G_intx)
#' }
#' 
#' 
#' #' Fit dwi data via GAM with global, impact, and interaction smooths accounting
#' #' for group differences. 
#' #' 
#' #' @param df Dataframe of AFQ data for single tract.
#' #' @param scalar_name DWI metric, dti_fa, dti_rd, dti_md, or dti_ad.
#' #' @param col_group Column of df holding group factor.
#' #' @param impact_meas Column name of impact measure.
#' #' @param ks_max Numeric, parameter for global smooth k argument.
#' #' @param ki_max Numeric, parameter for interaction smooth k argument.
#' #' @returns mgcv::bam fit object.
#' export("mod_gi_intx")
#' mod_gi_intx <- function(
#'     df, scalar_name, col_group, impact_meas, ks_max = 40, ki_max = 50) {
#'   # Validate scalar name
#'   if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
#'     stop("Unexpected scalar_name")
#'   }
#' 
#'   # Identify relevant columns
#'   fam_scalar <- .switch_family(scalar_name)
#'   names(df)[names(df) == scalar_name] <- "dti_scalar"
#'   names(df)[names(df) == col_group] <- "group"
#'   df$group <- factor(df$group)
#'   names(df)[names(df) == impact_meas] <- "impact_meas"
#' 
#'   # Fit data
#'   fit_GI_intx <- bam(
#'     dti_scalar ~ s(subj_id, bs = "re") +
#'       s(node_id, bs = "tp", k = ks_max) +
#'       s(impact_meas, by = group, bs = "tp", k = 5) +
#'       ti(
#'         node_id, impact_meas,
#'         by = group,
#'         bs = c("tp", "tp"), k = c(ki_max, 5)
#'       ),
#'     data = df,
#'     family = fam_scalar,
#'     method = "fREML",
#'     discrete = T,
#'     nthreads = 4
#'   )
#'   return(fit_GI_intx)
#' }
#' 
#' 
#' #' Fit dwi data via ordered HGAM with global, group smooths, and
#' #' interaction with Impact item smooths.
#' #' 
#' #' @param df Dataframe of AFQ data for single tract.
#' #' @param scalar_name DWI metric, dti_fa, dti_rd, dti_md, or dti_ad.
#' #' @param col_group Column of df holding group factor.
#' #' @param impact_meas Column name of impact measure.
#' #' @param ks_max Numeric, parameter for global smooth k argument.
#' #' @param ki_max Numeric, parameter for interaction smooth k argument.
#' #' @returns mgcv::bam fit object.
#' export("mod_gio_intx")
#' mod_gio_intx <- function(
#'     df, scalar_name, col_group, impact_meas, ks_max = 40, ki_max = 50) {
#'   # Validate scalar name
#'   if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
#'     stop("Unexpected scalar_name")
#'   }
#'   
#'   # Identify relevant columns
#'   fam_scalar <- .switch_family(scalar_name)
#'   names(df)[names(df) == scalar_name] <- "dti_scalar"
#'   names(df)[names(df) == col_group] <- "group"
#'   df$group <- factor(df$group)
#'   df$groupOF <- factor(df$group, ordered = T)
#'   names(df)[names(df) == impact_meas] <- "impact_meas"
#'   
#'   # Fit data
#'   fit_GIO_intx <- bam(
#'     dti_scalar ~ s(subj_id, bs = "re") +
#'       s(node_id, bs = "tp", k = ks_max) +
#'       ti(
#'         node_id, impact_meas,
#'         by = groupOF,
#'         bs = c("tp", "tp"), k = c(ki_max, 5)
#'       ),
#'     data = df,
#'     family = fam_scalar,
#'     method = "fREML",
#'     discrete = T,
#'     nthreads = 4
#'   )
#'   return(fit_GIO_intx)
#' }


#' Fit longitudinal HGAM with global, group smooths and wiggliness.
#' 
#' Pool within subject across multiple scans.
#' 
#' @param df Dataframe of AFQ data for single tract.
#' @param scalar_name DWI metric, dti_fa, dti_rd, dti_md, or dti_ad.
#' @returns mgcv::bam fit object.
export("mod_lgi")
mod_lgi <- function(df, scalar_name, k_max = 40) {
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


#' Fit longitudinal HGAM with global, group (ordered) smooths and wiggliness.
#' 
#' Pool within subject across multiple scans, and compare group B (or more) 
#' smooth to that of group A using ordered factors.
#' 
#' @param df Dataframe of AFQ data for single tract.
#' @param scalar_name DWI metric, dti_fa, dti_rd, dti_md, or dti_ad.
#' @returns mgcv::bam fit object.
export("mod_lgio")
mod_lgio <- function(df, scalar_name, k_max = 40) {
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


#' Fit longitudinal HGAM for all tracts of FA differences.
#' 
#' Manually calculate the interaction of factors tract_name and comp_scan,
#' then model all tract differences with group wiggliness. No global smooth
#' is used, so this is equivalent to the Pedersen 2019 model I. This 
#' effectively pools subject variance across both tract_name and scan_name,
#' while simplifying the nested factors. Differences are used to reduce the
#' requested number of smooths, reduce runtime, and maintain sufficient dof.
#' 
#' @param df Dataframe of AFQ with comp_scan and delta columns (output by 
#'  transform_data$calc_fa_delta).
#' @returns mgcv::bam fit object.
export("mod_ldi")
mod_ldi <- function(df) {
  df$tract_scan <- interaction(df$tract_name, df$comp_scan)
  fit_LDI <- bam(
    delta ~ s(subj_id, by = tract_scan, bs = "re") +
      s(node_id, by = tract_scan, bs = "tp", k = 40) +
      tract_name + comp_scan + tract_scan,
    data = df,
    family = gaussian(),
    method = "fREML",
    discrete = T,
    nthreads = 12
  )
  return(fit_LDI)
}

#' Fit longitudinal HGAM with global, group, and
#' interaction with Impact item smooths.
#'
#' TODO finalize specification.
export("mod_lgi_intx")
mod_lgi_intx <- function(
    df, impact_meas, 
    scalar_name = "dti_fa", ks_max = 40, ki_max = 50
) {
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
  fit_LGI_intx <- bam(
    dti_scalar ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = ks_max, m = 2) +
      s(imp_meas, by = scan_name, bs = "tp", k = 5) +
      ti(
        node_id, imp_meas,
        by = scan_name,
        bs = c("tp", "tp"), k = c(ki_max, 5),
        m = 1
      ),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 12
  )
  return(fit_LGSI_intx)
}


#' Fit longitudinal HGAM with global, group, and
#' interaction with Impact item smooths.
#'
#' TODO finalize specification.
export("mod_lgio_intx")
mod_lgio_intx <- function(
    df, 
    impact_meas, 
    scalar_name = "dti_fa", 
    ks_max = 40, 
    ki_max = 50
) {
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
  fit_LGIO_intx <- bam(
    dti_scalar ~ s(subj_id, scan_name, bs = "re") +
      s(node_id, bs = "tp", k = ks_max, m = 2) +
      s(imp_meas, by = scan_name, bs = "tp", k = 5) +
      ti(node_id, imp_meas, bs = c("tp", "tp"), k = c(ki_max, 5), m = 1) +
      ti(
        node_id, imp_meas,
        by = scanOF, bs = c("tp", "tp"), k = c(ki_max, 5), m = 1
      ),
    data = df,
    family = fam_scalar,
    method = "fREML",
    discrete = T,
    nthreads = 12
  )
  
  return(fit_LGIO_intx)
}
