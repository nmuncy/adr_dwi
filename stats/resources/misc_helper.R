# Miscellaneous functions to help with quick calculations, data
# extraction, and other tasks.

import(dplyr)
import(lubridate)
import(mgcViz)
import("tidyr")
import("tidyverse")
import("ggpubr")
import("stats", "reshape")
import(moments)


#' Identify GAM max deflections from zero.
#'
#' @param fit_gam mgcv::bam fit object.
#' @param idx_smooths Index of smooths in fit_gam to find deflections in.
#' @returns Dataframe containing node and deflection value for each smooth.
export("max_deflect")
max_deflect <- function(fit_gam, idx_smooths) {
  # Identify max deflections from zero
  t_name <- c_name <- n_name <- m_val <- c()
  p <- getViz(fit_gam)
  for (num in idx_smooths) {
    # Get plotting data and info
    p_info <- plot(sm(p, num))
    p_data <- as.data.frame(p_info$data$fit)
    max_y <- max(abs(p_data$y))

    # Get tract, comparison names
    row_name <- p_info$ggObj$labels$y
    h_str <- strsplit(row_name, split = ":")
    row_info <- strsplit(h_str[[1]][2], split = "\\.")

    # Update vecs for df building
    t_name <- c(t_name, row_info[[1]][1])
    c_name <- c(c_name, row_info[[1]][2])
    n_name <- c(n_name, p_data[which(abs(p_data$y) == max_y), ]$x)
    m_val <- c(m_val, max_y)
  }

  # Make dataframe, identify max, update names
  df_max <- data.frame(t_name, c_name, n_name, m_val)
  colnames(df_max) <- c("tract", "comp", "node", "max")
  df_max$tract <- gsub("tract_name", "", df_max$tract)
  df_max$tract <- gsub("tract_scan", "", df_max$tract)
  return(df_max)
}


#' Extract smooth node fit estimations.
#'
#' @param fit_gam mgcv::bam fit object.
#' @param idx_smooths Index of smooths in fit_gam to extract estimations from.
#' @returns Dataframe of estimations for each smooth.
export("get_estimations")
get_estimations <- function(fit_gam, idx_smooths) {
  # Identify max deflections from zero
  df_est <- data.frame(
    node = numeric(), est = numeric(), se = numeric(),
    comp = character(), tract = character()
  )
  p <- getViz(fit_gam)
  for (num in idx_smooths) {
    # Get plotting data and info
    p_info <- plot(sm(p, num))
    p_data <- as.data.frame(p_info$data$fit)
    colnames(p_data)[1:2] <- c("node", "est")

    # Get tract, comparison names
    row_name <- p_info$ggObj$labels$y
    h_str <- strsplit(row_name, split = ":")
    row_info <- strsplit(h_str[[1]][2], split = "\\.")
    p_data$comp <- row_info[[1]][2]
    p_data$tract <- row_info[[1]][1]

    # Stack dfs
    df_est <- rbind(df_est, p_data[, c(1, 2, 4, 5, 6)])
  }

  # Manage string values
  df_est$tract <- gsub("tract_name", "", df_est$tract)
  df_est$tract <- gsub("tract_scan", "", df_est$tract)
  return(df_est)
}


#' Change df based on subj_id and visit_name.x columns.
#'
#' @param df Tidy dataframe with columns subj_id, scan_name.
#' @param id (int) Subject id.
#' @param visit (str) Visit name.
#' @param cols (list) Column names to update
#' @param values (list) Values for insertion into df.
#' @returns Updated dataframe.
.chg_df <- function(df, id, visit, cols, values) {
  df[which(df$subj_id == id & df$scan_name == visit), cols] <- values
  return(df)
}


#' Manually manage fringe cases of matched impact-scan sessions.
#'
#' Impact data are too disorganized to realistically match
#' algorithmically an impact session and scan, despite
#' significant efforts to clean and match. Manually
#' adjust mappings of impact sessions with scans.
#'
#' @param df Long dataframe of scan post, rtp matched to impact session.
#' @returns Long dataframe with mappings.
export("fix_impact_scan")
fix_impact_scan <- function(df) {
  # Plan columns to fix and their respective values
  fix_cols <- c("impact_name", "impact_date", "num_tbi")
  empty_vals <- c(NA, NA, NA)

  # Manage missing RTP data.
  rtp_list <- c(3, 14, 43, 55, 58, 89, 96, 97, 119, 145, 147, 308, 9012)
  for (id in rtp_list) {
    df <- .chg_df(df, id, "rtp", fix_cols, empty_vals)
  }

  # Manage missing Post data.
  post_list <- c(42, 9027)
  for (id in c()) {
    df <- .chg_df(df, id, "post", fix_cols, empty_vals)
  }
  return(df)
}


#' Calculate FA post-base and rtp-base.
#'
#' @param df_afq Dataframe containing AFQ data.
#' @returns Dataframe with new columns comp_scan (comparison of scan names)
#'  and delta (A-B FA value).
export("calc_fa_delta")
calc_fa_delta <- function(df_afq) {
  # Subset relevant columns, convert to wide for easy A-B calcs
  df_sub <- subset(
    df_afq,
    select = c("subj_id", "scan_name", "tract_name", "node_id", "dti_fa")
  )
  df_wide <- reshape(
    df_sub,
    idvar = c("subj_id", "node_id", "tract_name"), timevar = "scan_name",
    direction = "wide"
  )
  df_wide$delta.post_base <- df_wide$dti_fa.post - df_wide$dti_fa.base
  df_wide$delta.rtp_base <- df_wide$dti_fa.rtp - df_wide$dti_fa.base

  # Return to long form
  df_wide <- subset(
    df_wide,
    select = c(
      "subj_id", "tract_name", "node_id", "delta.post_base", "delta.rtp_base"
    )
  )
  df <- reshape(
    df_wide,
    direction = "long",
    varying = c("delta.post_base", "delta.rtp_base"),
    times = c("post_base", "rtp_base"),
    idvar = c("subj_id", "tract_name", "node_id")
  )
  colnames(df)[4] <- c("comp_scan") # A-B column, comparison of scan names
  rownames(df) <- NULL
  df$comp_scan <- factor(df$comp_scan)
  rm(df_wide, df_sub)
  return(df)
}


#' Get index of LDI tract smooths.
#'
#' @param fit_obj mgcv::bam fit object.
#' @returns Named list, names=list of smooth names, post=index of post
#'    smooths, rtp=index of rtp smooths, all=index of post+rtp smooths.
export("idx_ldi_smooths")
idx_ldi_smooths <- function(fit_obj) {
  # Identify Post, RTP smooth indices and smooth names
  tract_smooths <- name_smooths <- c()
  for (num in 1:length(fit_obj$smooth)) {
    if (fit_obj[["smooth"]][[num]][["term"]] == "node_id") {
      tract_smooths <- c(tract_smooths, num)
      label <- fit_obj[["smooth"]][[num]][["label"]]
      h <- strsplit(label, "tract_scan")[[1]][2]
      name_smooths <- c(name_smooths, strsplit(h, "\\.")[[1]][1])
    }
  }

  # Get smooth groups
  half <- length(tract_smooths) / 2
  post_smooths <- utils::head(tract_smooths, half)
  rtp_smooths <- utils::tail(tract_smooths, half)
  name_smooths <- utils::head(name_smooths, half) # Names appear twice
  return(
    list(
      "names" = name_smooths,
      "post" = post_smooths,
      "rtp" = rtp_smooths,
      "all" = tract_smooths
    )
  )
}


#' Get index of DI tract smooths.
#'
#' @param fit_obj mgcv::bam fit object.
#' @returns Named list, names=list of smooth names, all=index of smooths.
export("idx_di_smooths")
idx_di_smooths <- function(fit_obj) {
  tract_smooths <- name_smooths <- c()
  for (num in 1:length(fit_obj$smooth)) {
    if (fit_obj[["smooth"]][[num]][["term"]] == "node_id") {
      tract_smooths <- c(tract_smooths, num)
      label <- fit_obj[["smooth"]][[num]][["label"]]
      name_smooths <- c(name_smooths, strsplit(label, "tract_name")[[1]][2])
    }
  }
  return(list("names" = name_smooths, "all" = tract_smooths))
}


#' Minimize date distance between dfs A, B.
#'
#' Identify which date in df B is closest to date
#' in df A, grouped by subj_id. Join df B to A.
#'
#' @param df_a Dataframe containing columns subj_id and date_a.
#' @param df_b Dataframe containing columns subj_id and date_b.
#' @param date_a (str) Column name of df_a containing datetime.
#' @param date_b (str) Column name of df_b containing datetime.
#' @returns Dataframe df_b joined to df_a by minimal datetime difference.
export("min_time")
min_time <- function(df_a, df_b, date_a, date_b) {
  df_a <- df_a %>% mutate(date = ymd(get(date_a)))
  df_b <- df_b %>% mutate(date = ymd(get(date_b)))

  df_a <- df_a %>%
    left_join(df_b, by = c("subj_id")) %>%
    mutate(time_diff = abs(date.x - date.y)) %>%
    group_by(subj_id, date.x) %>%
    arrange(time_diff) %>%
    slice(1) %>%
    ungroup()
  df_a <- subset(df_a, select = -c(date.x, date.y, time_diff))
  return(df_a)
}


#' Plan specific IMPACT measure for tract interaction analysis.
#'
#' @param tract String, name of AFQ tract.
#' @returns String, name of impact measure.
export("tract_impact")
tract_impact <- function(tract) {
  map_beh <- switch(tract,
    "Callosum Anterior Frontal" = "tot_symp",
    "Callosum Orbital" = "tot_symp",
    "Callosum Motor" = "rx_time",
    "Callosum Superior Parietal" = "mem_vis",
    "Callosum Posterior Parietal" = "mem_vis",
    "Callosum Occipital" = "mem_vis",
    "Left Anterior Thalamic" = "mem_vis",
    "Left Arcuate" = "mem_vis",
    "Left Cingulum Cingulate" = "mem_vis",
    "Left Corticospinal" = "rx_time",
    "Left Inferior Fronto-occipital" = "mem_vis",
    "Left Superior Longitudinal" = "mem_vis",
    "Right Anterior Thalamic" = "mem_vis",
    "Right Cingulum Cingulate" = "mem_vis",
    "Right Inferior Fronto-occipital" = "mem_vis",
    "Right Uncinate" = "tot_symp"
  )
  return(map_beh)
}


#' Provide ImPACT descriptive quantiles, skew, and kurtosis.
#' 
#' @param df_scan_imp Dataframe returned by workflows$get_data_scan_impact().
#' @returns Tibble table of descriptive stats.
export("desc_impact")
desc_impact <- function(df_scan_imp){
  # Organize relevant data
  df_stat <- subset(
    df_scan_imp, 
    select=c(
      subj_id, scan_name, mem_ver, mem_vis, 
      vis_mot, rx_time, imp_ctl, tot_symp
    )
  )
  df <- tidyr::gather(df_stat, imp_meas, value, mem_ver:tot_symp, factor_key=T)
  
  # Determine quantiles
  desc_stats <- df %>%
    group_by(imp_meas, scan_name) %>%
    summarize(
      min=min(value),
      q1=stats::quantile(value, 0.25),
      med=stats::median(value),
      q3=stats::quantile(value, 0.75),
      max=max(value),
      skew=round(moments::skewness(value), 2),
      kurt=round(moments::kurtosis(value), 2)
    )
  return(desc_stats)
}