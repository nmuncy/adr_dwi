# Various workflows required for differing analyses are organized
# into their respective functions. This allows for a less verbose
# stats script 'model_dwi'.

import(grDevices)
import(dplyr)
import(lubridate)
import("stats", "complete.cases")
import(mgcv)
import(mgcViz)
import(data.table)
import(gridExtra)
import(ggpubr)

draw_plots <- use("resources/draw_plots.R")
fit_gams <- use("resources/fit_gams.R")
pull_data <- use("resources/pull_data.R")
misc_help <- use("resources/misc_helper.R")
simul_data <- use("resources/simul_data.R")


#' Setup project analysis directory.
#'
#' Make select parent directories in stats_gams.
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


#' Fit and coordinate plotting of longitudinal HGAMs.
#'
#' Support longitudinal modeling and plotting of scalars. Makes longitudinal
#' GAMs with global and group smooths with group wiggliness (LGI) and also
#' global and group smooths as ordered factors (LGIO).
#'
#' Used to unload redundant work from gams_long_scalar().
#'
#' Models saved to out_dir/stats_gams/rda_objects/LGI<O>_tract/fit_LGI*
#' Summaries saved to out_dir/stats_gams/gam_summaries/LGI<O>_tract/fit_LGI*
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


#' Generate a graphic to visualize hypotheses and relate tract profiles
#' to smooths.
#'
#' Writes the figure stats_gams/plots/fit_hypotheses.png.
export("hyp_figure")
hyp_figure <- function() {
  
  # Make tract FA and RD data, with behavior
  set.seed(234)
  num_subj <- 80
  
  df_tract_fa <- df_tract_rd <- as.data.frame(matrix(nrow=0, ncol=6))
  col_names <- c("id", "node", "FA", "Visit", "subj", "Beh")
  colnames(df_tract_fa) <- col_names
  colnames(df_tract_rd) <- col_names
  
  for(subj in seq(from=1, to=num_subj)){
    subj_id <- paste0("s", subj)
    
    # FA data
    df_base_fa <- simul_data$simul_tract_beh("Base", "FA", subj_id)
    df_post_fa <- simul_data$simul_tract_beh(
      "Post", "FA", subj_id, adj_range = c(60, 80), adj_amount = -0.05)
    df_rtp_fa <- simul_data$simul_tract_beh(
      "RTP", "FA", subj_id, adj_range = c(60, 80), adj_amount = -0.025
    )
    df_tract_fa <- rbind(df_tract_fa, df_base_fa, df_post_fa, df_rtp_fa)
    
    # RD data
    df_base_rd <- simul_data$simul_tract_beh("Base", "RD", subj_id)
    df_post_rd <- simul_data$simul_tract_beh(
      "Post", "RD", subj_id, adj_range = c(60, 80), adj_amount = 0.05
    )
    df_rtp_rd <- simul_data$simul_tract_beh(
      "RTP", "RD", subj_id, adj_range = c(60, 80), adj_amount = 0.025
    )
    df_tract_rd <- rbind(df_tract_rd, df_base_rd, df_post_rd, df_rtp_rd)
  }
  
  # Clean up dfs
  df_tract_fa$Visit <- as.factor(df_tract_fa$Visit)
  df_tract_fa$subj <- as.factor(df_tract_fa$subj)
  rm(df_base_fa, df_post_fa, df_rtp_fa)
  
  df_tract_rd$id <- as.factor(df_tract_rd$id)
  df_tract_rd$Visit <- as.factor(df_tract_rd$Visit)
  df_tract_rd$subj <- as.factor(df_tract_rd$subj)
  rm(df_base_rd, df_post_rd, df_rtp_rd)
  
  # Plot FA an RD
  plots_tract <- draw_plots$grid_hyp_tracts(df_tract_fa, df_tract_rd)
  
  # # Plot Behavior
  # ggplot(data = df_tract_fa, aes(x = Visit, y = Beh)) +
  #   geom_point()
  
  # LGIO GAM and plots
  df_tract_fa$visit_OF <- factor(df_tract_fa$Visit, ordered = T)
  h_fam <- "gaussian()" # avoid 'gaussian' not found error
  fit_lgio <- bam(
    FA ~ s(subj, Visit, bs = "re") +
      s(node, bs = "tp", k = 15, m = 2) +
      s(node, by = visit_OF, bs = "tp", k = 15, m = 1),
    data = df_tract_fa,
    family = h_fam,
    method = "fREML",
    discrete = T,
    nthreads = 4
  )
  plots_lgio <- draw_plots$grid_hyp_lgio(
    fit_lgio, "Tract", "FA",
    num_G = 2, num_Ia = 3, num_Ib = 4,
    x_min = 0, x_max = 100
  )
  
  # Interaction (ordered) GAM and plots
  fit_lgio_intx <- bam(
    FA ~ s(subj, Visit, bs = "re") +
      s(node, bs = "tp", k = 15, m = 2) +
      s(Beh, by = Visit, bs = "tp", k = 5) +
      ti(node, Beh, bs = c("tp", "tp"), k = c(20, 5), m = 1) +
      ti(node, Beh, by = visit_OF, bs = c("tp", "tp"), k = c(20, 5), m = 1),
    data = df_tract_fa,
    family = h_fam,
    method = "fREML",
    discrete = T,
    nthreads = 12
  )
  plot_intx <- draw_plots$grid_hyp_intx(fit_lgio_intx)
  
  # Build figure
  grDevices::png(
    filename = paste0(
      .analysis_dir(), "/stats_gams/plots/fit_hypotheses.png"
    ),
    units = "in",
    height = 5,
    width = 15,
    res = 600
  )
  plots_final <- grid.arrange(
    arrangeGrob(plots_tract),
    arrangeGrob(plots_lgio),
    arrangeGrob(plot_intx),
    nrow = 1,
    ncol = 3,
    widths = c(1, 0.8, 1)
  )
  grDevices::dev.off()
}


#' Provide descriptive stats for ImPACT measures by visit.
#' 
#' @param df_scan_imp Dataframe returned by workflows$get_data_scan_impact().
export("beh_desc_impact")
beh_desc_impact <- function(df_scan_imp) {
  
  # Draw combined plot
  grDevices::png(
    filename = paste0(
      .analysis_dir(), "/stats_gams/plots/fit_impact_beh.png"
    ),
    units = "in",
    height = 4,
    width = 6,
    res = 600
  )
  draw_plots$grid_impact_beh(df_scan_imp)
  grDevices::dev.off()
  
}


#' Conduct GAMs for each ImPACT composite and total symptoms.
#'
#' Converts visit (Base, Post, RTP) to integer to model change in time (visit)
#' of ImPACT metrics. Each metric is fit with a relevant distribution and link
#' function, and scaling data occurs for zero/one-inflation.
#'
#' Summaries saved to
#' out_dir/stats_gams/gam_summaries/G_impact/fit_G_impact_*.txt
#' Plot saved to out_dir/stats_gams/plots/fit_impact.png.
#'
#' @param df_scan_imp Dataframe returned by workflows$get_data_scan_impact().
#' @returns Named list of GAM fits organized by ImPACT metric short name.
export("beh_gam_impact")
beh_gam_impact <- function(df_scan_imp) {
  # Make continuous x-axis
  df_scan_imp$scan_count <- 1
  df_scan_imp[which(df_scan_imp$scan_name == "post"), ]$scan_count <- 2
  df_scan_imp[which(df_scan_imp$scan_name == "rtp"), ]$scan_count <- 3

  # Set output strings
  out_dir <- paste0(.analysis_dir(), "/stats_gams/gam_summaries/G_impact")
  dir.create(file.path(out_dir), showWarnings = F)
  out_pref <- paste0(out_dir, "/fit_G_impact_")

  # Visual memory
  fit_mem_vis <- fit_gams$mod_imp(df_scan_imp, "mem_vis", fit_meth = "prop")
  fit_gams$write_gam_stats(fit_mem_vis, paste0(out_pref, "mem_vis.txt"))

  # Verbal memory
  fit_mem_ver <- fit_gams$mod_imp(
    df_scan_imp, "mem_ver",
    fit_meth = "prop", adj_value = -0.01
  )
  fit_gams$write_gam_stats(fit_mem_ver, paste0(out_pref, "mem_ver.txt"))

  # Visual motor
  fit_vis_mot <- fit_gams$mod_imp(df_scan_imp, "vis_mot") # Fit could be better
  fit_gams$write_gam_stats(fit_vis_mot, paste0(out_pref, "vis_mot.txt"))

  # Reaction time
  fit_rx_time <- fit_gams$mod_imp(df_scan_imp, "rx_time") # Fit could be better
  fit_gams$write_gam_stats(fit_rx_time, paste0(out_pref, "rx_time.txt"))

  # Impulse control
  fit_imp_ctl <- fit_gams$mod_imp(df_scan_imp, "imp_ctl", fit_meth = "negbin")
  fit_gams$write_gam_stats(fit_imp_ctl, paste0(out_pref, "imp_ctl.txt"))

  # Total symptoms
  fit_tot_symp <- fit_gams$mod_imp(df_scan_imp, "tot_symp", fit_meth = "negbin")
  fit_gams$write_gam_stats(fit_tot_symp, paste0(out_pref, "tot_symp.txt"))

  # Draw combined plot
  grDevices::png(
    filename = paste0(
      .analysis_dir(), "/stats_gams/plots/fit_impact_gam.png"
    ),
    units = "in",
    height = 4,
    width = 6,
    res = 600
  )
  draw_plots$grid_impact_gam(
    fit_mem_vis, fit_mem_ver, fit_vis_mot,
    fit_rx_time, fit_imp_ctl, fit_tot_symp
  )
  grDevices::dev.off()

  return(list(
    "mem_vis" = fit_mem_vis,
    "mem_ver" = fit_mem_ver,
    "vis_mot" = fit_vis_mot,
    "rx_time" = fit_rx_time,
    "imp_ctl" = fit_imp_ctl,
    "tot_symp" = fit_tot_symp
  ))
}


#' Pull and clean AFQ data.
#'
#' Get data from db_adr according to user request, where tbl_afq = 3 sessions
#' of ADR data (main dataset), tbl_afq_rerun = ADR baseline rerun through
#' pyAFQ (algorithimic reliability), or tbl_afq_rescan = single subject
#' scanned and processed multiple times (scan-rescan reliability).
#'
#' Notes:
#' - Clip tail nodes (x<10, x>89).
#'
#' @param table_name (String) Name of table holding pyAFQ metrics, tbl_afq,
#'  tbl_afq_rerun, or tbl_afq_rescan.
#' @returns Dataframe of AFQ tract metrics.
export("get_data_afq")
get_data_afq <- function(table_name) {
  # Validate user args
  if (!table_name %in% c("tbl_afq", "tbl_afq_rerun", "tbl_afq_rescan")) {
    stop(paste("Unexpected table_name:", tbl_name))
  }

  # Check for local csv, read-in data or pull from db_adr.
  afq_name <- strsplit(table_name, "tbl_")[[1]][2]
  afq_path <- paste(
    .analysis_dir(), "dataframes", paste0("df_", afq_name, ".csv"),
    sep = "/"
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

  if (table_name != "tbl_afq_rescan") {
    df_afq$scan_name <- factor(df_afq$scan_name)
    df_afq$scan_date <- as.POSIXct(
      df_afq$scan_date,
      format = "%Y-%m-%d", tz = "UTC"
    )
  } else {
    colnames(df_afq)[2] <- "scan_id"
    df_afq$scan_id <- as.character(as.numeric(df_afq$scan_id) + 1)
    df_afq$scan_id <- factor(df_afq$scan_id)
  }

  df_afq <- df_afq[which(df_afq$node_id > 9 & df_afq$node_id < 90), ]
  return(df_afq)
}


#' Get clean Impact and combine with AFQ data.
#'
#' Identify relevant impact visit for scan and then add composite
#' and total symptom metrics.
#'
#' @param df_afq Dataframe output of get_data_afq().
#' @returns Tidy dataframe of Impact and AFQ data.
export("get_data_scan_impact")
get_data_scan_impact <- function(df_afq) {
  # Get cleaned impact data, simply AFQ data.
  df_imp <- pull_data$clean_impact(.analysis_dir())
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
  df_scan_post <- misc_help$min_time(df_scan_post, df_imp_fu, "scan_date", "impact_date")
  df_scan_rtp <- df_afq[which(df_afq$scan_name == "rtp"), ]
  df_scan_rtp <- misc_help$min_time(df_scan_rtp, df_imp_fu, "scan_date", "impact_date")
  df_post <- rbind(df_scan_post, df_scan_rtp)
  rm(df_scan_rtp, df_scan_post, df_imp_fu)

  df_post <- misc_help$fix_impact_scan(df_post)
  df_post <- df_post[complete.cases(df_post$impact_date), ]
  df_post <- merge(
    x = df_post,
    y = df_imp[-idx_imp_base, ],
    by = c("subj_id", "impact_name", "impact_date", "num_tbi")
  )

  # Calculate days between scan date and impact date
  # df_post <- df_post[, c(1, 5:6, 2, 4, 3, 7:13)] # Match col order to df_base
  df_post <- df_post[names(df_base)]
  df_scan_imp <- rbind(df_base, df_post)

  df_scan_imp <- df_scan_imp %>%
    mutate(
      i_date = ymd(impact_date),
      s_date = ymd(scan_date),
      diff_scan_impact = as.numeric(s_date - i_date)
    )
  df_scan_imp <- subset(df_scan_imp, select = -c(i_date, s_date))

  # Check for local csv, read-in data or pull for db_adr.
  out_path <- paste(
    .analysis_dir(), "dataframes", "df_impact_scan.csv",
    sep = "/"
  )
  utils::write.csv(df_scan_imp, out_path, row.names = F)
  return(df_scan_imp)
}


#' Determine subject sex and participation.
#'
#' Identify subjects with scan data and add demographic
#' and ImpACT values where available. Determine age
#' metrics, supply session counts for scan and impact.
#'
#' @param df_afq Dataframe output of get_data_afq().
#' @param df_scan_imp Dataframe output of get_data_scan_impact().
#' @returns Named list containing demographic metrics and counts.
export("get_demo_counts")
get_demo_counts <- function(df_afq, df_scan_imp) {
  # Get demographic data
  demo_path <- paste(
    .analysis_dir(), "dataframes", "df_demographics.csv",
    sep = "/"
  )
  if (!file.exists(demo_path)) {
    df <- pull_data$get_demographics()
    utils::write.csv(df, demo_path, row.names = F)
    rm(df)
  }
  df_demo <- utils::read.csv(demo_path)
  df_demo$subj_id <- factor(as.character(df_demo$subj_id))
  df_demo$sex <- factor(df_demo$sex)

  # Identify subjs with scan data
  tract_list <- unique(df_afq$tract_name)
  df_afq_sub <- df_afq[which(
    df_afq$node == 10 & df_afq$tract_name == tract_list[1]
  ), ]

  # Combine demo, scan info
  df <- merge(
    x = df_afq_sub, y = df_demo, by = "subj_id", all.x = T
  )
  rm(df_afq_sub, df_demo)
  df <- subset(
    df,
    select = c("subj_id", "scan_name", "sex", "age_base", "dti_fa")
  )

  # Add impact
  df_imp <- subset(df_scan_imp, select = c(subj_id, scan_name, mem_ver))
  df <- merge(
    df, df_imp,
    by = c("subj_id", "scan_name"),
    all.x = T
  )
  rm(df_imp)

  # Identify unique subjects
  idx_subj <- match(unique(df$subj_id), df$subj_id)
  df_subj <- df[idx_subj, ]

  # Identify subjects with scan, impact data
  idx_scan <- which(!is.na(df$dti_fa))
  idx_imp <- which(!is.na(df$mem_ver))

  # # Identify subjs with post but no base (and converse)
  # idx_post <- which(df$scan_name == "post")
  # idx_base <- which(df$scan_name == "base")

  return(list(
    "num_tot" = length(df_subj$subj_id),
    "num_male" = length(which(df_subj$sex == "M")),
    "num_female" = length(which(df_subj$sex == "F")),
    "sex_scan" = table(df[idx_scan, ]$sex, df[idx_scan, ]$scan_name),
    "sex_imp" = table(df[idx_imp, ]$sex, df[idx_imp, ]$scan_name),
    # "num_post_no_base" = dim(anti_join(df[idx_post,], df[idx_base,], by="subj_id"))[1],
    # "num_base_no_post" = dim(anti_join(df[idx_base,], df[idx_post,], by="subj_id"))[1],
    "age_avg" = round(mean(df_subj$age_base), 2),
    "age_std" = round(stats::sd(df_subj$age_base), 2),
    "age_min" = min(df$age_base),
    "age_max" = max(df$age_base)
  ))
}


#' Model all tracts for post-base and rtp-base differences (delta).
#'
#' Conduct longitudinal HGAM with all tracts and scan times so subject variance
#' is pooled across tracts and scans. Delta values are used as ordered factors
#' become difficult across factor interactions.
#'
#' Models saved to out_dir/stats_gams/rda_objects/fit_LDI_fa.Rda
#' Summaries saved to out_dir/stats_gams/gam_summaries/fit_LDI_fa.txt
#' Plot saved to out_dir/stats_gams/plots/LDI_all/fit_LDI_*
#'
#' @param df_afq Dataframe containing AFQ data, from
#'  workflows$get_data_afq("tbl_afq").
#' @param make_plots Logical, whether to draw all tract grids.
#' @returns mgcv::bam fit object.
export("dwi_gam_delta_all")
dwi_gam_delta_all <- function(df_afq, make_plots = T) {
  analysis_dir <- .analysis_dir()

  # Calculate delta and run model
  df <- misc_help$calc_fa_delta(df_afq)
  rds_ldi <- paste0(analysis_dir, "/stats_gams/rda_objects/fit_LDI_fa.Rda")
  if (!file.exists(rds_ldi)) {
    h_gam <- fit_gams$mod_ldi(df)
    saveRDS(h_gam, file = rds_ldi)
    rm(h_gam)
  }
  fit_LDI <- readRDS(rds_ldi)

  # Generate grids of post-base and rtp-base.
  if (make_plots == FALSE) {
    return(fit_LDI)
  }

  # Mine, print summary stats
  sum_ldi <- paste0(analysis_dir, "/stats_gams/gam_summaries/fit_LDI_fa.txt")
  if (!file.exists(sum_ldi)) {
    fit_gams$write_gam_stats(fit_LDI, sum_ldi)
  }

  # Get indices of tract smooths and their names
  idx_smooths <- misc_help$idx_ldi_smooths(fit_LDI)

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
  df_max <- misc_help$max_deflect(fit_LDI, idx_smooths$all)
  out_csv <- paste0(
    analysis_dir, "/stats_gams/gam_summaries/fit_LDI_fa_max.csv"
  )
  utils::write.csv(df_max, out_csv, row.names = F)

  # Draw individual tract smooths
  c <- 1 # Use counter to align name with post and rtp tract smooths
  while (c <= length(idx_smooths$names)) {
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


#' Model all tracts for run-rerun FA differences (delta).
#'
#' Test for algorithmic variance by comparing tractometric profiles
#' from rerunning PyAFQ on Base session (test-retest). Includes difference
#' FA values for all tracts.
#'
#' Models saved to out_dir/stats_gams/rda_objects/fit_DI_rerun_fa.Rda
#' Summaries saved to out_dir/stats_gams/gam_summaries/fit_DI_rerun_fa*
#' Plot saved to out_dir/stats_gams/plots/DI_all/fit_DI_rerun_all*
#'
#' @param df_afq Dataframe containing AFQ data, from workflows$get_data_afq().
#' @param df_afq Dataframe from workflows$get_data_afq("tbl_afq_rerun").
#' @returns mgcv::bam fit object.
export("dwi_gam_delta_rerun")
dwi_gam_delta_rerun <- function(df_afq, df_afq_rr) {
  # Subset df_afq for relevant values
  df_afq_base <- df_afq[which(df_afq$scan_name == "base"), ]
  df_afq_base <- subset(
    df_afq_base,
    select = c("subj_id", "tract_name", "node_id", "dti_fa")
  )
  colnames(df_afq_base)[4] <- "fa_base1"
  row.names(df_afq_base) <- NULL

  # Match df_afq_rr for merging
  df_rerun_base <- subset(
    df_afq_rr,
    select = c("subj_id", "tract_name", "node_id", "dti_fa")
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

  # Mine, print summary stats
  sum_di <- paste0(
    analysis_dir,
    "/stats_gams/gam_summaries/fit_DI_rerun_fa.txt"
  )
  if (!file.exists(sum_di)) {
    fit_gams$write_gam_stats(fit_DI, sum_di)
  }

  # Identify max deflections from zero
  idx_smooths <- misc_help$idx_di_smooths(fit_DI)
  df_max <- misc_help$max_deflect(fit_DI, idx_smooths$all)
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


#' Model tract node-FA-time interaction.
#'
#' Calculate RTP-Post FA change value, model delta FA with smooths for node,
#' number of days between RTP and Post, and produce tensor product interaction
#' smooth for 2D interaction.
#'
#' Models saved to out_dir/stats_gams/rda_objects/DI_time/fit_DI_time*
#' Summaries saved to out_dir/stats_gams/gam_summaries/DI_time/fit_DI_time_*
#' Plot saved to out_dir/stats_gams/plots/DI_time/fit_DI_time*
#'
#' @param df_afq Dataframe containing AFQ data, from
#'  workflows$get_data_afq("tbl_afq").
#' @param tract Name of PyAFQ tract.
export("dwi_gam_delta_time")
dwi_gam_delta_time <- function(df_afq, tract) {
  # Tract FA for post, rtp
  df_tract <- df_afq[which(df_afq$tract_name == tract), ]
  df_tract <- subset(df_tract, select = -c(dti_md, dti_ad, dti_rd))
  df_tract <- df_tract[which(df_tract$scan_name != "base"), ]

  # Organize for difference calcs
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
  
  # Determine distribution of recovery times
  df_rec <- df[which(df$node_id == 10), ]
  table(df_rec$days.rtp_post)
  df_rec <- df[which(df$days.rtp_post < 40 & df$node_id == 10), ]
  rec_avg <- round(mean(df_rec$days.rtp_post), 2)
  rec_std <- round(stats::sd(df_rec$days.rtp_post), 2)
  num_gt_14 <- length(which(df_rec$days.rtp_post > 14))

  # Drop extra-long recovery times (very few, wrecks model fits).
  df <- df[which(df$days.rtp_post < 40), ]

  # Fit and save model
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

  # Write summary
  sum_di <- paste0(
    .analysis_dir(), "/stats_gams/gam_summaries/DI_time/fit_DI_time_",
    tract_short, "_fa.txt"
  )
  fit_gams$write_gam_stats(fit_DI_time, sum_di)

  # Draw plots
  plot_obj <- getViz(fit_DI_time)
  plot_time <- draw_plots$draw_di_time(plot_obj, tract)

  plot_dir <- paste0(.analysis_dir(), "/stats_gams/plots/DI_time")
  dir.create(file.path(plot_dir), showWarnings = F)
  ggplot2::ggsave(
    filename = paste0(plot_dir, "/fit_DI_time_", tract_short, "_fa.png"),
    plot = plot_time$time_diff,
    units = "in",
    height = 4,
    width = 6,
    dpi = 600
  )
}


#' Fit DWI scalars with longitudinal HGAMs for single tract.
#'
#' Fit DWI data (FA, RD, AD, MD) with longitudinal hierarchical
#' GAMS using both global and group smooths (and group wiggliness).
#' Also fit as ordered factor for group (post/rtp vs base).
#'
#' Models saved to out_dir/stats_gams/rda_objects/LGI<O>_tract/fit_LGI*
#' Summaries saved to out_dir/stats_gams/gam_summaries/LGI<O>_tract/fit_LGI*
#' Plot saved to out_dir/stats_gams/plots/LGI<O>_tract/fit_LGI*
#'
#' @param df_afq Dataframe output of clean_afq().
#' @param tract String, name of AFQ tract (corresponds to df_afq$tract_name).
#' @returns Nested named with FA, AD, RD, MD objects for GAM fit (gam_LGI),
#'    ordered GAM fit (gam_LGIO), and LGIO plots (gam_plots).
export("dwi_gam_long_tract")
dwi_gam_long_tract <- function(df_afq, tract) {
  # Subset dataframe by tract
  df <- df_afq[which(df_afq$tract_name == tract), ]

  # Get GAMs and plots for each scalar
  obj_FA <- .fit_plot_long_tract(df, tract, "dti_fa")
  obj_MD <- .fit_plot_long_tract(df, tract, "dti_md")
  obj_RD <- .fit_plot_long_tract(df, tract, "dti_rd")
  obj_AD <- .fit_plot_long_tract(df, tract, "dti_ad")

  # Make LGIO stat table
  sum_to_table <- function(fit_obj, scalar) {
    sum_table <- summary(fit_obj)
    table_lgio <- as.data.frame(sum_table$s.table)
    table_lgio <- cbind(smooth = rownames(table_lgio), table_lgio)
    rownames(table_lgio) <- 1:nrow(table_lgio)
    table_lgio$scalar <- scalar
    table_lgio[2:5] <- round(table_lgio[2:5], 2)
    return(table_lgio)
  }
  stat_table <- sum_to_table(obj_FA$fit_LGIO, "FA")
  stat_table <- rbind(stat_table, sum_to_table(obj_MD$fit_LGIO, "MD"))
  stat_table <- rbind(stat_table, sum_to_table(obj_AD$fit_LGIO, "AD"))
  stat_table <- rbind(stat_table, sum_to_table(obj_RD$fit_LGIO, "RD"))

  h_tract <- fit_gams$switch_tract(tract)
  out_table <- paste0(
    .analysis_dir(),
    "/stats_gams/gam_summaries/LGIO_tract/summary_LGIO_", h_tract, ".csv"
  )
  utils::write.csv(stat_table, out_table, row.names = F)

  # Assemble and write LGIO plots
  grDevices::png(
    filename = paste0(
      .analysis_dir(), "/stats_gams/plots/LGIO_tract/fit_LGIO_", h_tract, ".png"
    ),
    units = "in",
    height = 10,
    width = 8,
    res = 600
  )
  plot_grid <- draw_plots$draw_scalar_grid(
    obj_FA$plots_LGIO, obj_MD$plots_LGIO,
    obj_AD$plots_LGIO, obj_RD$plots_LGIO,
    tract
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
    obj_AD$plots_LGI, obj_RD$plots_LGI,
    tract
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
    ),
    plot_grid = plot_grid
  ))
}


#' Model tract node-FA-ImPACT metric interaction.
#'
#' Conduct LGI and LGIO interaction models of node-FA-ImPACT interactions.
#' FA is fit with 1D smooths for node, ImPACT, and 2D tensor product smooths
#' of node X ImPACT, for each visit (Base, Post, and RTP).
#'
#' Models saved to out_dir/stats_gams/rda_objects/DI_time/fit_DI_time*
#' Summaries saved to out_dir/stats_gams/gam_summaries/DI_time/fit_DI_time_*
#' Plot saved to out_dir/stats_gams/plots/DI_time/fit_DI_time*
#'
#' @param df_afq Dataframe containing AFQ data, from
#'  workflows$get_data_afq("tbl_afq").
#' @param df_scan_imp Dataframe returned by workflows$get_data_scan_impact().
#' @param tract Name of PyAFQ tract.
#' @param impact_meas Optional, if "planned" then pre-planned ImPACT metrics
#'  will be tested for select tracts. Otherwise specify short name of ImPACT.
#'  @return Named list of LGI and LGIO interaction models.
export("dwi_gam_long_impact")
dwi_gam_long_impact <- function(
    df_afq, df_scan_imp, tract, impact_meas = "planned") {
  # Manage pre-planned or user-specified impact measure
  if (impact_meas == "planned") {
    impact_meas <- misc_help$tract_impact(as.character(tract))
  }

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


#' Plot all smooths from dwi_gam_delta_all and dwi_gam_delta_rerun models.
#'
#' Stitch together plots of delta_all and delta_rerun. Plot saved to
#' out_dir/stats_gams/plots/fit_LDI_DI_rerun.png.
#'
#' @param fit_LDI GAM object returned by workflows$dwi_gam_delta_all().
#' @param fit_DI_rr GAM object returned by workflows$dwi_gam_delta_rerun().
export("plot_dwi_gam_all_rerun")
plot_dwi_gam_all_rerun <- function(fit_LDI, fit_DI_rr) {
  # Determine LDI smooth indices and make grid
  idx_ldi <- misc_help$idx_ldi_smooths(fit_LDI)
  grid_ldi <- draw_plots$grid_ldi_comb(
    fit_LDI, idx_ldi$post, idx_ldi$rtp, idx_ldi$names
  )

  # Determine DI smooth indices and make grid
  idx_di <- misc_help$idx_di_smooths(fit_DI_rr)
  grid_di <- draw_plots$grid_di_comb(fit_DI_rr, idx_di$all, idx_di$names)

  # Combine grids
  grDevices::png(
    filename = paste0(
      .analysis_dir(), "/stats_gams/plots/fit_LDI_DI_rerun.png"
    ),
    units = "in",
    height = 8,
    width = 12,
    res = 600
  )
  grid.arrange(grid_ldi, grid_di, heights = c(1, 0.5))
  grDevices::dev.off()
}
