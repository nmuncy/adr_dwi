import(grDevices)
import(dplyr)
import(lubridate)
import("stats", "complete.cases")
import(mgcViz)
import(data.table)

draw_plots <- use("resources/draw_plots.R")
fit_gams <- use("resources/fit_gams.R")
pull_data <- use("resources/pull_data.R")
quick_stats <- use("resources/quick_stats.R")
transform_data <- use("resources/transform_data.R")


#' Setup project analysis directory.
#'
#' @returns Path to NRDStor project analysis directory for
#' Gimli (linux) and Frodo (mac) workstations.
.analysis_dir <- function() {
  if (Sys.info()["sysname"] == "Linux") {
    an_dir <- "/run/user/1001/gvfs/smb-share:server=nrdstor.unl.edu,share=nrdstor/muncylab/nmuncy2/ADR/analyses"
  } else if (Sys.info()["sysname"] == "Darwin") {
    an_dir <- "/Volumes/nrdstor/muncylab/nmuncy2/ADR/analyses"
  }
  dir.create(file.path(an_dir, "dataframes"), showWarnings = F)
  dir.create(file.path(an_dir, "stats_gams/gam_summaries"), showWarnings = F)
  dir.create(file.path(an_dir, "stats_gams/rda_objects"), showWarnings = F)
  dir.create(file.path(an_dir, "stats_gams/plots"), showWarnings = F)
  return(an_dir)
}


#' Pull and clean impact data.
#'
#' Update impact names and add days post FU1 (diff_post) for
#' and subsequent impact assessments.
#'
#' @returns Dataframe of impact composite scores.
.clean_impact <- function() {
  # Check for local csv, read-in or pull from db_adr
  imp_path <- paste(
    .analysis_dir(), "dataframes", "df_impact.csv",
    sep = "/"
  )

  if (!file.exists(imp_path)) {
    df <- pull_data$get_user_comp()
    utils::write.csv(df, imp_path, row.names = F)
    rm(df)
  }
  df_imp <- utils::read.csv(imp_path)

  # Manage column types, names
  df_imp$subj_id <- factor(df_imp$subj_id)
  df_imp$impact_name <- factor(df_imp$impact_name)
  df_imp$impact_date <- as.POSIXct(
    df_imp$impact_date,
    format = "%Y-%m-%d", tz = "UTC"
  )
  colnames(df_imp)[5:10] <- c(
    "mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl", "tot_symp"
  )

  # Calculate days since fu1
  idx_base <- which(df_imp$impact_name == "base")
  df_base <- df_imp[idx_base, ]
  df_post <- df_imp[-c(idx_base), ]
  rm(df_imp)
  df_post <- df_post %>%
    group_by(subj_id, num_tbi) %>%
    mutate(
      date = ymd(impact_date),
      diff_post = as.numeric(date - min(date))
    )
  df_post <- subset(df_post, select = -c(date))
  # names(df_post)[names(df_post) == 'date'] <- 'impact_date'

  # Return combined dfs
  df_base$diff_post <- NA
  df_comb <- rbind(df_base, df_post)
  rm(df_base, df_post)
  return(df_comb)
}


#' Pull and clean AFQ data.
#'
#' Clip tail nodes (x<10, x>89).
#'
#' @param table_name (String) Name of table holding pyAFQ metrics, tbl_afq
#'  or tbl_afq_rerun.
#' @returns Dataframe of AFQ tract metrics.
export("clean_afq")
clean_afq <- function(table_name) {
  # Validate user args
  if (!table_name %in% c("tbl_afq", "tbl_afq_rerun", "tbl_afq_rescan")) {
    stop(paste("Unexpected table_name:", tbl_name))
  }

  # Check for local csv, read-in data or pull from db_adr.
  afq_name <- strsplit(table_name, "tbl_")[[1]][2]
  afq_path <- paste(
    .analysis_dir(), "dataframes", paste0("df_", afq_name, ".csv"), sep = "/"
  )

  if (!file.exists(afq_path)) {
    df <- pull_data$get_afq(table_name = table_name)
    utils::write.csv(df, afq_path, row.names = F)
    rm(df)
  }
  df_afq <- utils::read.csv(afq_path)

  # Drop specific tracts
  drop_tracts <- c(
    "Left Posterior Arcuate",
    "Right Posterior Arcuate",
    "Left Vertical Occipital",
    "Right Vertical Occipital"
  )
  df_afq <- df_afq[-which(df_afq$tract_name %in% drop_tracts), ]
  row.names(df_afq) <- NULL

  # Manage column types, clip tails
  df_afq$subj_id <- factor(df_afq$subj_id)
  df_afq$tract_name <- factor(df_afq$tract_name)
  
  if (table_name != "tbl_afq_rescan"){
    df_afq$scan_name <- factor(df_afq$scan_name)
    df_afq$scan_date <- as.POSIXct(
      df_afq$scan_date,
      format = "%Y-%m-%d", tz = "UTC"
    )
  } else{
    colnames(df_afq)[2] <- "scan_id"
    df_afq$scan_id <- as.character(as.numeric(df_afq$scan_id) + 1)
    df_afq$scan_id <- factor(df_afq$scan_id)
  }
  
  df_afq <- df_afq[which(df_afq$node_id > 9 & df_afq$node_id < 90), ]
  return(df_afq)
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
.min_time <- function(df_a, df_b, date_a, date_b) {
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


#' Get clean Impact and AFQ data.
#'
#' Identify relevant impact visit for scan,
#' add composites for scan.
#'
#' @returns Tidy dataframe of Impact and AFQ data.
export("get_scan_impact")
get_scan_impact <- function() {
  # Get cleaned data.
  df_imp <- .clean_impact()
  df_afq <- clean_afq("tbl_afq")

  # Extract scan dates and names
  df_afq <- df_afq[
    which(
      df_afq$node_id == 10 & df_afq$tract_name == "Callosum Anterior Frontal"
    ),
    c("subj_id", "scan_name", "scan_date")
  ]

  # Match impact base to scan base.
  idx_imp_base <- which(df_imp$impact_name == "base")
  idx_afq_base <- which(df_afq$scan_name == "base")

  df_base <- merge(
    x = df_afq[idx_afq_base, ],
    y = df_imp[idx_imp_base, ],
    by = "subj_id",
    all.x = T
  )
  df_base <- df_base[complete.cases(df_base$impact_date), ]

  # Find closest impact fu date for scan post and rtp, and fix
  # matching issues between impact and scan.
  df_imp_fu <- df_imp[
    -idx_imp_base,
    c("subj_id", "impact_name", "impact_date", "num_tbi")
  ]

  df_scan_post <- df_afq[which(df_afq$scan_name == "post"), ]
  df_scan_post <- .min_time(df_scan_post, df_imp_fu, "scan_date", "impact_date")
  df_scan_rtp <- df_afq[which(df_afq$scan_name == "rtp"), ]
  df_scan_rtp <- .min_time(df_scan_rtp, df_imp_fu, "scan_date", "impact_date")
  df_post <- rbind(df_scan_post, df_scan_rtp)
  rm(df_scan_rtp, df_scan_post, df_imp_fu)

  df_post <- transform_data$fix_impact_scan(df_post)
  df_post <- df_post[complete.cases(df_post$impact_date), ]
  df_post <- merge(
    x = df_post,
    y = df_imp[-idx_imp_base, ],
    by = c("subj_id", "impact_name", "impact_date", "num_tbi")
  )

  # Calculate days between scan date and impact date
  df_post <- df_post[, c(1, 5:6, 2, 4, 3, 7:13)] # Match col order to df_base
  df_scan_imp <- rbind(df_base, df_post)

  df_scan_imp <- df_scan_imp %>%
    mutate(
      i_date = ymd(impact_date),
      s_date = ymd(scan_date),
      diff_scan_impact = as.numeric(s_date - i_date)
    )
  df_scan_imp <- subset(df_scan_imp, select = -c(i_date, s_date))

  # Check for local csv, ead-in data or pull for db_adr.
  out_path <- paste(
    .analysis_dir(), "dataframes", "df_impact_scan.csv",
    sep = "/"
  )
  utils::write.csv(df_scan_imp, out_path, row.names = F)

  return(df_scan_imp)
}


#' Generate impact better-worse data plots.
#'
#' Organize impact measures by groups of subjects who had better
#' post scores than base, and those who had worse scores. Track
#' individual subjects and also compute descriptive and
#' non-parametrics stats.
#'
#' @param df Dataframe of impact data, output of get_scan_impact().
export("impact_better_worse")
impact_better_worse <- function(df) {
  for (col_name in c(
    "tot_symp", "mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl"
  )
  ) {
    low <- if (col_name %in% c("rx_time", "tot_symp")) T else F
    df_sub <- transform_data$compare_base_post(col_name, df, low = low)
    print(draw_plots$visit_track(col_name, df_sub))
  }
}


#' Title.
#' TODO
export("basic_demographics")
basic_demographics <- function(){
  demo_path <- paste(
    .analysis_dir(), "dataframes", "df_demographics.csv", sep = "/"
  )
  
  if (!file.exists(demo_path)) {
    df <- pull_data$get_demographics()
    utils::write.csv(df, demo_path, row.names = F)
    rm(df)
  }
  df_demo <- utils::read.csv(demo_path)
  df_demo$subj_id <- factor(as.character(df_demo$subj_id))
  df_demo$sex <- factor(df_demo$sex)
  
  #
  df_afq <- clean_afq("tbl_afq")
  tract_list <- unique(df_afq$tract_name)
  df_afq <- df_afq[which(
    df_afq$node == 10 & df_afq$tract_name == tract_list[1]
  ), ]
  
  #
  df <- merge(
    x = df_afq, y = df_demo, by = "subj_id", all.x = T
  )
  rm(df_afq, df_demo)
  df <- subset(df, select = c("subj_id", "scan_name", "sex", "age_base"))
  
  #
  return(list(
    "sex_scan" = table(df$sex, df$scan_name),
    "age_avg" = round(mean(df$age_base), 2),
    "age_std" = round(stats::sd(df$age_base), 2)
  ))
}


#' Identify subjects of K-means groups.
#'
#' Identify which subjects are classified in the various k-means groups.
#' As k-means applies group labels at random, hardcode known members
#' for consistent group membership.
#'
#' Currently only supports k-means with 3 groups (for use with post data) when
#' investigating mem_vis, mem_ver, vis_mot, and rx_time.
#'
#' @param df_clust Dataframe generated from run_kmeans$clust_km.
#' @param col_list Vector column numbers corresponding to Impact measures.
#' @returns Dataframe with added column km_grp.
.k_grp <- function(df_clust, col_list) {
  # Organize group labels for consistency, by known members of groups
  if (length(col_list) == 4) {
    grp_c_lab <- df_clust[which(df_clust$subj_id == 216), ]$km_clust
    grp_b_lab <- df_clust[which(df_clust$subj_id == 110), ]$km_clust
    grp_a_lab <- df_clust[which(df_clust$subj_id == 267), ]$km_clust

    df_clust$km_grp <- 1
    df_clust[which(df_clust$km_clust == grp_b_lab), ]$km_grp <- 2
    df_clust[which(df_clust$km_clust == grp_c_lab), ]$km_grp <- 3
  }
  return(df_clust)
}


#' Check Impact data for clustering.
#'
#' Conduct principal component and k-means clustering analyses
#' for specified scan_name. Currently only includes mem_vis, mem_ver,
#' vis_mot, and rx_time Impact responses.
#'
#' @param df_scan_imp Dataframe, output of get_scan_impact().
#' @param scan_name String, Name of scan: base, post, or rtp.
#' @returns Named list containing a df with impact groups (df_sik), stats
#'    for kmeans and pca (stats_km, stats_pc), and plots for kmeans and
#'    pca (plot_km, plot_pc).
export("impact_cluster")
impact_cluster <- function(df_scan_imp, scan_name) {
  if (!scan_name %in% c("base", "post", "rtp")) {
    stop("Unexpected scan_name")
  }

  # Subset dataframe and set column, cluter values
  df <- df_scan_imp[which(df_scan_imp$scan_name == scan_name), ]
  col_list <- c(7:10) # Impact meas items, exclude imp_ctl & tot_symp
  num_k <- 3

  # Conduct PCA and make plots
  stats_pc <- quick_stats$run_pca(df, col_list)
  plot_pc <- draw_plots$draw_pca(stats_pc)

  # Conduct K-means and make plots
  stats_km <- quick_stats$run_kmeans(df, col_list, num_k)
  clust_km <- stats_km$clust_km
  data_norm <- stats_km$data_norm
  plot_km <- draw_plots$draw_kmean(stats_km$data_norm, stats_km$clust_km)
  # table(stats_km$clust_km)

  # Convert clustering to dataframe
  df_clust <- as.data.frame(stats_km$clust_km)
  rownames(df_clust) <- df$subj_id
  df_clust <- cbind(subj_id = rownames(df_clust), df_clust)
  rownames(df_clust) <- NULL
  colnames(df_clust)[2] <- "km_clust"

  # Add grouping to impact
  df_clust <- .k_grp(df_clust, col_list)
  df <- merge(
    x = df,
    y = df_clust,
    by = "subj_id",
    all = T
  )

  return(list(
    "df_sik" = df,
    "stats_pc" = stats_pc,
    "plot_pc" = plot_pc,
    "stats_km" = stats_km,
    "plot_km" = plot_km
  ))
}



#' Model all tracts for post-base and rtp-base differences (delta).
#'
#' Conduct longitudinal HGAM with all tracts and scan times so subject variance
#' is pooled across tracts and scans.
#'
#' @param df_afq Dataframe containing AFQ data.
#' @param make_plots Logical, whether to draw all tract grids.
#' @returns mgcv::bam fit object.
export("gam_delta_long_all")
gam_delta_long_all <- function(df_afq, make_plots = T) {
  analysis_dir <- .analysis_dir()

  # Calculate delta and run model
  df <- transform_data$calc_fa_delta(df_afq)
  rds_ldi <- paste0(analysis_dir, "/stats_gams/rda_objects/fit_LDI_fa.Rda")
  if (!file.exists(rds_ldi)) {
    h_gam <- fit_gams$mod_ldi(df)
    saveRDS(h_gam, file = rds_ldi)
    rm(h_gam)
  }
  fit_LDI <- readRDS(rds_ldi)
  
  # Generate grids of post-base and rtp-base.
  if(make_plots == FALSE){
    return(fit_LDI)
  }

  # Mine, print summary stats
  sum_ldi <- paste0(analysis_dir, "/stats_gams/gam_summaries/fit_LDI_fa.txt")
  if (!file.exists(sum_ldi)) {
    fit_gams$write_gam_stats(fit_LDI, sum_ldi)
  }

  # Get indices of tract smooths and their names
  idx_smooths <- transform_data$idx_ldi_smooths(fit_LDI)
  
  # Draw combined plot
  grDevices::png(
    filename = paste0(
      analysis_dir, "/stats_gams/plots/LDI_all/fit_LDI_all.png"
    ),
    units = "in",
    height = 8,
    width = 12,
    res = 600
  )
  draw_plots$grid_ldi_comb(
    fit_LDI, idx_smooths$post, idx_smooths$rtp, idx_smooths$names
  )
  grDevices::dev.off()
  
  # Identify max deflections from zero
  df_max <- quick_stats$max_deflect(fit_LDI, idx_smooths$all)
  out_csv <- paste0(
    analysis_dir, "/stats_gams/gam_summaries/fit_LDI_fa_max.csv"
  )
  utils::write.csv(df_max, out_csv, row.names = F)

  c <- 1 # Use counter to align name with post and rtp tract smooths
  while (c < length(idx_smooths$names)) {
    h_tract <- fit_gams$switch_tract(idx_smooths$names[c])
    grDevices::png(
      filename = paste0(
        analysis_dir, "/stats_gams/plots/LDI_all/fit_LDI_", h_tract, ".png"
      ),
      units = "in",
      height = 8,
      width = 6,
      res = 600
    )
    draw_plots$grid_ldi(
      fit_LDI, idx_smooths$names[c], "FA", 
      idx_smooths$post[c], idx_smooths$rtp[c]
    )
    grDevices::dev.off()
    c <- c + 1
  }
  return(fit_LDI)
}



#' Title.
#' TODO
export("gam_delta_rerun_all")
gam_delta_rerun_all <- function(df_afq, df_afq_rr){
  # Subset df_afq for relevant values
  df_afq_base <- df_afq[which(df_afq$scan_name == "base"), ]
  df_afq_base <- subset(
    df_afq_base, select = c("subj_id", "tract_name", "node_id", "dti_fa")
  )
  colnames(df_afq_base)[4] <- "fa_base1"
  row.names(df_afq_base) <- NULL
  
  # Match df_afq_rr for merging
  df_rerun_base <- subset(
    df_afq_rr, select = c("subj_id", "tract_name", "node_id", "dti_fa")
  )
  colnames(df_rerun_base)[4] <- "fa_base2"
  row.names(df_rerun_base) <- NULL
  
  # Get difference of FAs between runs
  df <- merge(
    df_afq_base, df_rerun_base, 
    by = c("subj_id", "tract_name", "node_id"), 
    all = T
  )
  rm(df_afq_base, df_rerun_base)
  df$delta <- df$fa_base2 - df$fa_base1 # Similar comparison to post-base
  
  # Run model
  analysis_dir <- .analysis_dir()
  rds_di <- paste0(analysis_dir, "/stats_gams/rda_objects/fit_DI_rerun_fa.Rda")
  if (!file.exists(rds_di)) {
    h_gam <- fit_gams$mod_di(df)
    saveRDS(h_gam, file = rds_di)
    rm(h_gam)
  }
  fit_DI <- readRDS(rds_di)
  idx_smooths <- transform_data$idx_di_smooths(fit_DI)
  
  # Identify max deflections from zero
  df_max <- quick_stats$max_deflect(fit_DI, idx_smooths$all)
  df_max$comp <- "run-rerun"
  out_csv <- paste0(
    .analysis_dir(), "/stats_gams/gam_summaries/fit_DI_rerun_fa_max.csv"
  )
  utils::write.csv(df_max, out_csv, row.names = F)
  
  # Draw combined plot
  grDevices::png(
    filename = paste0(
      .analysis_dir(), "/stats_gams/plots/DI_all/fit_DI_rerun_all.png"
    ),
    units = "in",
    height = 6,
    width = 12,
    res = 600
  )
  draw_plots$grid_di_comb(fit_DI, idx_smooths$all, idx_smooths$names)
  grDevices::dev.off()
  return(fit_DI)
}


#' Title.
#' TODO
export("gam_delta_rescan_all")
gam_delta_rescan_all <- function(df_afq_rs){
  analysis_dir <- .analysis_dir()
  
  # Mimic scan column from study data for use with existing functions.
  df_afq_rs$scan_name <- NA
  df_afq_rs[which(df_afq_rs$scan_id == "1"), ]$scan_name <- "base"
  df_afq_rs[which(df_afq_rs$scan_id == "2"), ]$scan_name <- "post"
  df_afq_rs[which(df_afq_rs$scan_id == "3"), ]$scan_name <- "rtp"
  df_afq_rs$scan_name <- factor(df_afq_rs$scan_name)
  df_afq_rs <- subset(df_afq_rs, select = -c(scan_id))
  
  # Calculate delta and run model
  df <- transform_data$calc_fa_delta(df_afq_rs)
  rm(df_afq_rs)
  
  # Convert back to scan-rescan column names
  df$comp <- NA
  df[which(df$comp_scan == "post_base"), ]$comp <- "S2-S1"
  df[which(df$comp_scan == "rtp_base"), ]$comp <- "S3-S1"
  df <- subset(df, select = -c(comp_scan))
  colnames(df)[5] <- "comp_scan"
  df$comp_scan <- factor(df$comp_scan)
  
  #
  rds_ldi <- paste0(analysis_dir, "/stats_gams/rda_objects/fit_LDI_rescan_fa.Rda")
  if (!file.exists(rds_ldi)) {
    h_gam <- fit_gams$mod_ldi_rescan(df)
    saveRDS(h_gam, file = rds_ldi)
    rm(h_gam)
  }
  fit_LDI <- readRDS(rds_ldi)
  
  # Mine, print summary stats
  sum_ldi <- paste0(analysis_dir, "/stats_gams/gam_summaries/fit_LDI_rescan_fa.txt")
  if (!file.exists(sum_ldi)) {
    fit_gams$write_gam_stats(fit_LDI, sum_ldi)
  }
  
  # Get indices of tract smooths and their names, rather
  # than write a new function just use existing and know that
  # post = 2-1, rtp = 3-1.
  idx_smooths <- transform_data$idx_ldi_smooths(fit_LDI)
  
  # Draw combined plot
  grDevices::png(
    filename = paste0(
      analysis_dir, "/stats_gams/plots/LDI_all/fit_LDI_rescan_all.png"
    ),
    units = "in",
    height = 8,
    width = 12,
    res = 600
  )
  draw_plots$grid_ldi_comb(
    fit_LDI, idx_smooths$post, idx_smooths$rtp, idx_smooths$names,
    comp_a = "S2-S1", comp_b = "S3-S1"
  )
  grDevices::dev.off()
  
  # Identify max deflections from zero
  df_max <- quick_stats$max_deflect(fit_LDI, idx_smooths$all)
  out_csv <- paste0(
    analysis_dir, "/stats_gams/gam_summaries/fit_LDI_rescan_fa_max.csv"
  )
  utils::write.csv(df_max, out_csv, row.names = F)
  return(fit_LDI)
}


#' Title.
#' 
#' TODO
export("gam_delta_tract_time")
gam_delta_tract_time <- function(df_afq, tract){
  
  # Tract FA for post, rtp
  df_tract <- df_afq[which(df_afq$tract_name == tract), ]
  df_tract <- subset(df_tract, select = -c(dti_md, dti_ad, dti_rd))
  df_tract <- df_tract[which(df_tract$scan_name != "base"), ]
  
  #
  df <- stats::reshape(
    df_tract,
    idvar = c("subj_id", "tract_name", "node_id"),
    timevar = "scan_name",
    direction = "wide"
  )
  df$days.rtp_post <- df$scan_date.rtp - df$scan_date.post
  df$delta.rtp_post <- df$dti_fa.rtp - df$dti_fa.post
  df <- subset(
    df, 
    select = c(subj_id, tract_name, node_id, days.rtp_post, delta.rtp_post)
  )
  df$days.rtp_post <- as.numeric(df$days.rtp_post)
  df <- df[complete.cases(df$days.rtp_post), ]
  rm(df_tract)
  
  # hist(df$days.rtp_post)
  df <- df[which(df$days.rtp_post < 40), ] #
  
  #
  # fit_DI_time <- fit_gams$mod_di_time(df)
  
  rda_dir <- paste0(.analysis_dir(), "/stats_gams/rda_objects/DI_time")
  dir.create(file.path(rda_dir), showWarnings = F)
  tract_short <- fit_gams$switch_tract(tract)
  rds_di <- paste0(
    rda_dir, "/fit_DI_time_", tract_short, "_fa.Rda"
  )
  if (!file.exists(rds_di)) {
    h_gam <- fit_gams$mod_di_time(df)
    saveRDS(h_gam, file = rds_di)
    rm(h_gam)
  }
  fit_DI_time <- readRDS(rds_di)
  
  #
  plot_obj <- getViz(fit_DI_time)
  plot_time <- draw_plots$draw_di_time(plot_obj, tract)
  
  #
  plot_dir <- paste0(.analysis_dir(), "/stats_gams/plots/DI_time")
  dir.create(file.path(plot_dir), showWarnings = F)
  ggplot2::ggsave(
    filename = paste0(plot_dir, "/fit_DI_time_", tract_short, "_fa.png"),
    plot = plot_time$time_diff,
    units = "in",
    height = 8,
    width = 8,
    dpi = 600
  )
}


#' TODO
#' 
export("gam_fa_rebase_all")
gam_fa_rebase_all <- function(df_afq){
  
  #
  df <- df_afq[which(df_afq$scan_name == "base"), ]
  set.seed(123)
  subj_ids <- sample(unique(as.character(df$subj_id)))
  subj_a <- subj_ids[1:34]
  subj_b <- subj_ids[35:67]
  
  df$scan_name <- NA
  df[which(df$subj_id %in% subj_a), ]$scan_name <- "base1"
  df[which(df$subj_id %in% subj_b), ]$scan_name <- "base2"
  df$scan_name <- factor(df$scan_name)
  df <- subset(df, select = -c(scan_date, dti_md, dti_ad, dti_rd))
  
  #
  library(data.table)
  df <- df[which(df$tract_name %like% "Callosum"), ]
  df$tract_name <- factor(as.character(df$tract_name))
  
}


#' Fit and coordinate plotting of longitudinal HGAMs.
#'
#' Support longitudinal modeling and plotting of scalars. Makes longitudinal
#' GAMs with global and group smooths with group wiggliness (LGI) and also
#' global and group smooths as ordered factors (LGIO).
#'
#' Used to unload redundant work from gams_long_scalar().
#'
#' @param df Dataframe containing AFQ data.
#' @param tract String, name of AFQ tract (corresponds to df$tract_name).
#' @param scalar_name String, name of DTI scalar (corresponds to df column).
#' @returns Named list organized by GAM fits (fit_LGI, fit_LGIO)
#'    and the smooth plots (plot).
.fit_plot_long_tract <- function(df, tract, scalar_name) {
  # Validate user args, get tract/scalar names
  if (!scalar_name %in% paste0("dti_", c("fa", "rd", "md", "ad"))) {
    stop("Unexpected scalar_name")
  }
  # print(scalar_name)
  h_tract <- fit_gams$switch_tract(tract)
  scalar <- strsplit(scalar_name, "_")[[1]][2]

  # Get (and make/save if needed) LGI model.
  analysis_dir <- .analysis_dir()
  rds_lgi <- paste0(
    analysis_dir, "/stats_gams/rda_objects/LGI_tract/fit_LGI_",
    h_tract, "_", scalar, ".Rda"
  )
  if (!file.exists(rds_lgi)) {
    h_gam <- fit_gams$mod_lgi(df, scalar_name)
    saveRDS(h_gam, file = rds_lgi)
    rm(h_gam)
  }
  fit_LGI <- readRDS(rds_lgi)
  sum_lgi <- paste0(
    analysis_dir, "/stats_gams/gam_summaries/LGI_tract/fit_LGI_",
    h_tract, "_", scalar, ".txt"
  )
  fit_gams$write_gam_stats(fit_LGI, sum_lgi)

  # Get (and make/save if needed) LGIO model, write summary stats to disk.
  rds_lgio <- paste0(
    analysis_dir, "/stats_gams/rda_objects/LGIO_tract/fit_LGIO_",
    h_tract, "_", scalar, ".Rda"
  )
  if (!file.exists(rds_lgio)) {
    h_gam <- fit_gams$mod_lgio(df, scalar_name)
    saveRDS(h_gam, file = rds_lgio)
    rm(h_gam)
  }
  fit_LGIO <- readRDS(rds_lgio)
  sum_lgio <- paste0(
    analysis_dir, "/stats_gams/gam_summaries/LGIO_tract/fit_LGIO_",
    h_tract, "_", scalar, ".txt"
  )
  fit_gams$write_gam_stats(fit_LGIO, sum_lgio)
  # gam.check(fit_LGIO)
  # summary(fit_LGIO)
  # plot(fit_LGIO)

  # Coordinate drawing of smooths
  plots_LGI <- draw_plots$grid_lgi(
    fit_LGI, tract, toupper(scalar)
  )
  plots_LGIO <- draw_plots$grid_lgio(
    fit_LGIO, tract, toupper(scalar)
  )
  return(list(
    "fit_LGI" = fit_LGI,
    "fit_LGIO" = fit_LGIO,
    "plots_LGIO" = plots_LGIO,
    "plots_LGI" = plots_LGI
  ))
}


#' Fit DWI scalars with longitudinal HGAMs for single tract.
#'
#' Fit DWI data (FA, RD, AD, MD) with longitudinal hierarchical
#' GAMS using both global and group smooths (and group wiggliness).
#' Also fit as ordered factor for group (post/rtp vs base).
#'
#' @param df_afq Dataframe output of clean_afq().
#' @param tract String, name of AFQ tract (corresponds to df_afq$tract_name).
#' @returns Nested named with FA, AD, RD, MD objects for GAM fit (gam_LGI),
#'    ordered GAM fit (gam_LGIO), and LGIO plots (gam_plots).
export("gams_long_tract")
gams_long_tract <- function(df_afq, tract) {
  # Subset dataframe by tract
  df <- df_afq[which(df_afq$tract_name == tract), ]

  # Get GAMs and plots for each scalar
  obj_FA <- .fit_plot_long_tract(df, tract, "dti_fa")
  obj_MD <- .fit_plot_long_tract(df, tract, "dti_md")
  obj_RD <- .fit_plot_long_tract(df, tract, "dti_rd")
  obj_AD <- .fit_plot_long_tract(df, tract, "dti_ad")

  # Assemble and write LGIO plots
  h_tract <- fit_gams$switch_tract(tract)
  grDevices::png(
    filename = paste0(
      .analysis_dir(), "/stats_gams/plots/LGIO_tract/fit_LGIO_", h_tract, ".png"
    ),
    units = "in",
    height = 10,
    width = 8,
    res = 600
  )
  draw_plots$draw_scalar_grid(
    obj_FA$plots_LGIO, obj_MD$plots_LGIO,
    obj_AD$plots_LGIO, obj_RD$plots_LGIO
  )
  grDevices::dev.off()

  # Assemble and write LGI plots
  grDevices::png(
    filename = paste0(
      .analysis_dir(), "/stats_gams/plots/LGI_tract/fit_LGI_", h_tract, ".png"
    ),
    units = "in",
    height = 10,
    width = 8,
    res = 600
  )
  draw_plots$draw_scalar_grid(
    obj_FA$plots_LGI, obj_MD$plots_LGI,
    obj_AD$plots_LGI, obj_RD$plots_LGI
  )
  grDevices::dev.off()

  return(list(
    gam_LGI = list(
      "FA" = obj_FA$fit_LGI, "RD" = obj_RD$fit_LGI,
      "AD" = obj_AD$fit_LGI, "MD" = obj_MD$fit_LGI
    ),
    gam_LGIO = list(
      "FA" = obj_FA$fit_LGIO, "RD" = obj_RD$fit_LGIO,
      "AD" = obj_AD$fit_LGIO, "MD" = obj_MD$fit_LGIO
    ),
    gam_plots = list(
      "FA" = obj_FA$plots_LGIO, "RD" = obj_RD$plots_LGIO,
      "AD" = obj_AD$plots_LGIO, "MD" = obj_MD$plots_LGIO
    )
  ))
}


#' Plan specific IMPACT measure for tract interaction analysis.
#'
#' @param tract String, name of AFQ tract.
#' @returns String, name of impact measure.
.tract_impact <- function(tract) {
  map_beh <- switch(tract,
    "Callosum Orbital" = "tot_symp",
    "Left Anterior Thalamic" = "tot_symp",
    "Left Arcuate" = "mem_ver",
    "Left Cingulum Cingulate" = "mem_vis",
    "Left Corticospinal" = "rx_time",
    "Left Inferior Fronto-occipital" = "mem_vis",
    "Right Cingulum Cingulate" = "mem_vis",
    "Right Inferior Fronto-occipital" = "mem_vis",
    "Right Uncinate" = "tot_symp"
  )
  return(map_beh)
}


#' Fit DWI scalars X Impact with longitudinal HGAMs.
#'
#' TODO
export("gams_long_tract_intx")
gams_long_tract_intx <- function(df_afq, df_scan_imp, tract) {
  # # Validate user args
  # if (!impact_meas %in%
  #     c("mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl", "tot_symp")
  # ) {
  #   stop("Unexpected impact_meas")
  # }
  impact_meas <- .tract_impact(as.character(tract))

  #  Subset tract data and add impact measure
  df <- df_afq[which(df_afq$tract_name == tract), ]
  df <- merge(
    x = df,
    y = df_scan_imp[, c("subj_id", "scan_name", impact_meas)],
    by = c("subj_id", "scan_name"),
    all.x = T
  )

  # Set up for writing objects
  h_tract <- fit_gams$switch_tract(tract)
  scalar <- strsplit("dti_fa", "_")[[1]][2]
  analysis_dir <- .analysis_dir()

  # Get (and make/save if needed) LGI_intx, LGIO_intx models.
  rds_lgi <- paste0(
    analysis_dir, "/stats_gams/rda_objects/LGI_intx_tract/fit_LGI_intx_",
    h_tract, "_", scalar, "_", impact_meas, ".txt"
  )
  if (!file.exists(rds_lgi)) {
    h_gam <- fit_gams$mod_lgi_intx(df, impact_meas)
    saveRDS(h_gam, file = rds_lgi)
    rm(h_gam)
  }
  fit_LGI_intx <- readRDS(rds_lgi)

  rds_lgio <- paste0(
    analysis_dir, "/stats_gams/rda_objects/LGIO_intx_tract/fit_LGIO_intx_",
    h_tract, "_", scalar, "_", impact_meas, ".txt"
  )
  if (!file.exists(rds_lgio)) {
    h_gam <- fit_gams$mod_lgio_intx(df, impact_meas)
    saveRDS(h_gam, file = rds_lgio)
    rm(h_gam)
  }
  fit_LGIO_intx <- readRDS(rds_lgio)

  # Write summaries
  sum_lgi <- paste0(
    analysis_dir, "/stats_gams/gam_summaries/LGI_intx_tract/fit_LGI_intx_",
    h_tract, "_", scalar, ".txt"
  )
  fit_gams$write_gam_stats(fit_LGI_intx, sum_lgi)
  sum_lgio <- paste0(
    analysis_dir, "/stats_gams/gam_summaries/LGIO_intx_tract/fit_LGIO_intx_",
    h_tract, "_", scalar, "_", impact_meas, ".txt"
  )
  fit_gams$write_gam_stats(fit_LGIO_intx, sum_lgio)

  # Make LGI_intx, LGIO_intx plots
  grDevices::png(
    filename = paste0(
      analysis_dir, "/stats_gams/plots/LGI_intx_tract/fit_LGI_intx_",
      h_tract, "_", impact_meas, ".png"
    ),
    units = "in",
    height = 8,
    width = 6,
    res = 600
  )
  draw_plots$grid_lgi_intx(fit_LGI_intx, tract, toupper(scalar), impact_meas)
  grDevices::dev.off()

  grDevices::png(
    filename = paste0(
      analysis_dir, "/stats_gams/plots/LGIO_intx_tract/fit_LGIO_intx_",
      h_tract, "_", impact_meas, ".png"
    ),
    units = "in",
    height = 8,
    width = 6,
    res = 600
  )
  draw_plots$grid_lgio_intx(fit_LGIO_intx, tract, toupper(scalar), impact_meas)
  grDevices::dev.off()

  return(list(
    gam_LGI_intx = fit_LGI_intx,
    gam_LGIO_intx = fit_LGIO_intx
  ))
}


#' Plot fit estimates and max.
#' 
#' TODO
export("plot_estimates")
plot_estimates <- function(fit_LDI, fit_DI_rr, fit_LDI_rs){
  # Node estimates and max from study data
  idx_ldi <- transform_data$idx_ldi_smooths(fit_LDI)
  df_est_study <- quick_stats$get_estimations(fit_LDI, idx_ldi$all)
  df_max_study <- quick_stats$max_deflect(fit_LDI, idx_ldi$all)
  
  # Node estimates and max from pyAFQ rerun
  idx_di_rr <- transform_data$idx_di_smooths(fit_DI_rr)
  df_est_rr <- quick_stats$get_estimations(fit_DI_rr, idx_di_rr$all)
  df_est_rr$comp <- "run_rerun"
  df_max_rr <- quick_stats$max_deflect(fit_DI_rr, idx_di_rr$all)
  df_max_rr$comp <- "run_rerun"
  
  # Node estimations and max from rescan
  idx_ldi_rs <- transform_data$idx_ldi_smooths(fit_LDI_rs)
  df_est_rs <- quick_stats$get_estimations(fit_LDI_rs, idx_ldi_rs$all)
  df_max_rs <- quick_stats$max_deflect(fit_LDI_rs, idx_ldi_rs$all)
  
  # Concatenate estimations
  # df_est <- rbind(df_est_study, df_est_rr, df_est_rs)
  df_est <- rbind(df_est_study, df_est_rr)
  rm(df_est_study, df_est_rr, df_est_rs)
  df_est$comp <- factor(df_est$comp)
  df_est$tract <- factor(df_est$tract)
  df_est$lb <- as.numeric(df_est$est - (1.96 * df_est$se))
  df_est$ub <- as.numeric(df_est$est + (1.96 * df_est$se))
  
  # Concatenate max
  # df_max <- rbind(df_max_study, df_max_rr, df_max_rs)
  df_max <- rbind(df_max_study, df_max_rr)
  rm(df_max_study, df_max_rr, df_max_rs)
  df_max$comp <- factor(df_max$comp)
  df_max$tract <- factor(df_max$tract)
  
  # Boxplot of difference estimations by group of tracts
  df_est_cc <- df_est[df_est$tract %like% "Callosum", ]
  df_est_cc$tract <- gsub("Callosum ", "", df_est_cc$tract)
  ggplot(
    data = df_est_cc,
    aes(x = .data$tract, y=.data$est)
  ) +
    geom_boxplot(aes(fill=.data$comp)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  df_est_left <- df_est[df_est$tract %like% "Left", ]
  df_est_left$tract <- gsub("Left ", "", df_est_left$tract)
  ggplot(
    data = df_est_left,
    aes(x = .data$tract, y=.data$est)
  ) +
    geom_boxplot(aes(fill=.data$comp)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  df_est_right <- df_est[df_est$tract %like% "Right", ]
  df_est_right$tract <- gsub("Right ", "", df_est_right$tract)
  ggplot(
    data = df_est_right,
    aes(x = .data$tract, y=.data$est)
  ) +
    geom_boxplot(aes(fill=.data$comp)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  rm(df_est_cc, df_est_left, df_est_right)
  
  # Lineplot of max differences by group of tracts
  df_max_cc <- df_max[df_max$tract %like% "Callosum", ]
  df_max_cc$tract <- gsub("Callosum ", "", df_max_cc$tract)
  ggplot(
    data = df_max_cc,
    aes(x = .data$tract, y=.data$max, colour=.data$comp, group=.data$comp)
  ) +
    geom_line() +
    geom_point() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  df_max_left <- df_max[df_max$tract %like% "Left", ]
  df_max_left$tract <- gsub("Left ", "", df_max_left$tract)
  ggplot(
    data = df_max_left,
    aes(x = .data$tract, y=.data$max, colour=.data$comp, group=.data$comp)
  ) +
    geom_line() +
    geom_point() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  df_max_right <- df_max[df_max$tract %like% "Right", ]
  df_max_right$tract <- gsub("Right ", "", df_max_right$tract)
  ggplot(
    data = df_max_right,
    aes(x = .data$tract, y=.data$max, colour=.data$comp, group=.data$comp)
  ) +
    geom_line() +
    geom_point() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  rm(df_max_cc, df_max_left, df_max_right)
  
  
  #
  tract <- "Callosum Motor"
  df_est_tract <- df_est[which(df_est$tract == tract), ]
  ggplot(
    data = df_est_tract,
    aes(x = .data$node, y = .data$est, group = .data$comp)
  ) +
    geom_hline(yintercept = 0) +
    geom_line(aes(color = .data$comp)) +
    geom_ribbon(aes(ymin = .data$lb, ymax=.data$ub), alpha = 0.2) +
    scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
    scale_color_discrete(name = "") +
    theme(
      text = element_text(family = "Times New Roman"),
      legend.text = element_text(size = 10)
    )
  
}


#' Fit Post DWI scalars with HGAMs, grouped by k-means.
#'
#' TODO finalize or remove.
#'
#' Investigate scalars from post scans, accounting for k-means grouping
#' (output of impact_cluster()).
#'
#' @param df_afq Dataframe, output of clean_afq().
#' @param tract String, name of AFQ tract (corresponds to df_afq$tract_name).
#' @param df_scan_imp Dataframe, output of get_scan_impact().
#' @param imp_clust Named list, output of impact_cluster().
export("gams_post_kmeans")
gams_post_kmeans <- function(df_afq, tract, df_scan_imp, imp_clust) {
  # Callosum Temporal shows kmeans group differences on mem_ver in post,
  # but minimal differences on vis_mot.
  # Left Inferior Fronto-occipital was flat across effects.

  #
  imp_clust <- workflows$impact_cluster(df_scan_imp, "post")
  subj_exp <- imp_clust$df_sik[which(imp_clust$df_sik$km_grp != 1), ]$subj_id

  tract <- "Callosum Temporal"
  df <- df_afq[which(df_afq$tract_name == tract & df_afq$scan_name == "post"), ]
  df$group <- "con"
  df[which(df$subj_id %in% subj_exp), ]$group <- "exp"

  #
  fit_GSI <- fit_gams$gam_gsi(df, "dti_rd", "group")
  fit_GSIO <- fit_gams$gam_gsio(df, "dti_rd", "group")

  #
  impact_meas <- "mem_vis"
  df <- merge(
    x = df,
    y = df_scan_imp[, c("subj_id", "scan_name", impact_meas)],
    by = c("subj_id", "scan_name"),
    all.x = T
  )

  #
  fit_G_intx <- fit_gams$mod_g_intx(df, "dti_fa", impact_meas)
  plot(fit_G_intx)
  p <- getViz(fit_G_intx)
  plot(p)

  #
  fit_GI_intx <- fit_gams$mod_gi_intx(df, "dti_fa", "group", impact_meas)
  plot(fit_GI_intx)
  p <- getViz(fit_GI_intx)
  plot(p)

  fit_GIO_intx <- fit_gams$mod_gio_intx(df, "dti_fa", "group", impact_meas)
  plot(fit_GIO_intx)
  p <- getViz(fit_GIO_intx)
  plot(p)
  #
}



#' #' Run GAM with randomized sample
#' #'
#' export("gam_rand")
#' gam_rand <- function(df, seed) {
#'   # Identify subjs with base and post
#'   subj_list <- unique(df[which(df$scan_name == "post"), ]$subj_id)
#'   df <- df[which(df$subj_id %in% subj_list), ]
#'
#'   # Rand subjs
#'   set.seed(seed)
#'   subj_list <- sample(subj_list)
#'   grp_a <- subj_list[1:32]
#'   grp_b <- subj_list[33:65]
#'
#'   # Replace dti_fa of post for grp_a with base of grp_b
#'   idx_a_post <- which(df$scan_name == "post" & df$subj_id %in% grp_a)
#'   idx_b_base <- which(df$scan_name == "base" & df$subj_id %in% grp_b)
#'   df[idx_a_post, ]$dti_fa <- df[idx_b_base, ]$dti_fa
#'
#'   # Run GAM
#'   fit_rand <- gam_spar(df)
#'   return(fit_rand)
#' }
