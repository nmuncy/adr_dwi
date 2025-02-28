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


#' Pull and clean AFQ data.
#'
#' Clip tail nodes (x<10, x>89).
#'
#' @param table_name (String) Name of table holding pyAFQ metrics, tbl_afq
#'  or tbl_afq_rerun.
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


#' Get clean Impact and combine wtih AFQ data.
#'
#' Identify relevant impact visit for scan,
#' add composites for scan.
#'
#' @param df_afq Dataframe containing AFQ data.
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



#' Title.
#' TODO
export("get_demographics")
get_demographics <- function() {
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

  #
  df_afq <- get_afq("tbl_afq")
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


#' Title
#'
#' TODO
export("count_scan_impact")
count_scan_impact <- function(df_afq, df_scan_imp) {
  #
  df_scan <-
    df_afq[which(df_afq$node_id == 10 & df_afq$tract_name == "Right Arcuate"), ]
  df_scan <- subset(df_scan, select = c(subj_id, scan_name, dti_fa))
  df_imp <- subset(df_scan_imp, select = c(subj_id, scan_name, mem_ver))

  df <- merge(
    df_scan, df_imp,
    by = c("subj_id", "scan_name"),
    all.x = T
  )
  rm(df_scan, df_imp)

  #
  sess_count <- list()
  for (sess in c("base", "post", "rtp")) {
    sess_count[paste0(sess, "_scan")] <-
      length(which(df$scan_name == sess & !is.na(df$dti_fa)))
    sess_count[paste0(sess, "_imp")] <-
      length(which(df$scan_name == sess & !is.na(df$mem_ver)))
  }
  return(sess_count)

  # library(DiagrammeR)
  # library(glue)
  #
  # grViz(
  #   diagram = "digraph flowchart {
  #     node [
  #       fontname = times,
  #       fontsize = 9,
  #       shape = rounded,
  #       penwidth = 1.0
  #     ]
  #     graph[nodesep = 0.5]
  #     tab1 [label = '@@1']
  #     tab2 [label = '@@2']
  #     tab3 [label = '@@3']
  #     tab1 -> tab2;
  #     tab2 -> tab3;
  #   }
  #
  #   [1]: 'Base (N_scan=67N_imp=61)'
  #   [2]: 'Post (N_scan=65N_imp=48)'
  #   [3]: 'RTP (N_scan=56N_imp=32)'"
  # )
}


#' Title.
#'
#' TODO
export("impact_gams")
impact_gams <- function(df_scan_imp) {
  # Make continuous x-axis
  df_scan_imp$scan_count <- 1
  df_scan_imp[which(df_scan_imp$scan_name == "post"), ]$scan_count <- 2
  df_scan_imp[which(df_scan_imp$scan_name == "rtp"), ]$scan_count <- 3

  # Model each impact measure, capture summary
  out_dir <- paste0(.analysis_dir(), "/stats_gams/gam_summaries/G_impact")
  dir.create(file.path(out_dir), showWarnings = F)
  out_pref <- paste0(out_dir, "/fit_G_impact_")

  fit_mem_vis <- fit_gams$mod_imp(df_scan_imp, "mem_vis", fit_meth = "prop")
  fit_gams$write_gam_stats(fit_mem_vis, paste0(out_pref, "mem_vis.txt"))

  fit_mem_ver <- fit_gams$mod_imp(
    df_scan_imp, "mem_ver",
    fit_meth = "prop", adj_value = -0.01
  )
  fit_gams$write_gam_stats(fit_mem_ver, paste0(out_pref, "mem_ver.txt"))

  fit_vis_mot <- fit_gams$mod_imp(df_scan_imp, "vis_mot") # Fit could be better
  fit_gams$write_gam_stats(fit_vis_mot, paste0(out_pref, "vis_mot.txt"))

  fit_rx_time <- fit_gams$mod_imp(df_scan_imp, "rx_time") # Fit could be better
  fit_gams$write_gam_stats(fit_rx_time, paste0(out_pref, "rx_time.txt"))

  fit_imp_ctl <- fit_gams$mod_imp(df_scan_imp, "imp_ctl", fit_meth = "negbin")
  fit_gams$write_gam_stats(fit_imp_ctl, paste0(out_pref, "imp_ctl.txt"))

  fit_tot_symp <- fit_gams$mod_imp(df_scan_imp, "tot_symp", fit_meth = "negbin")
  fit_gams$write_gam_stats(fit_tot_symp, paste0(out_pref, "tot_symp.txt"))


  # Draw combined plot
  grDevices::png(
    filename = paste0(
      .analysis_dir(), "/stats_gams/plots/fit_impact.png"
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


  # #
  # beh_list <- colnames(df_scan_imp)[7:12]
  # beh <- beh_list[6]
  # 
  # library(mgcv)
  # library(fitdistrplus)
  # library(itsadug)
  # descdist(df_scan_imp[, beh])
  # 
  # hist(df_scan_imp[, beh], breaks=30)
  # 
  # fit_beh <- bam(
  #   tot_symp ~ s(scan_count, bs = "tp", k = 3) +
  #     s(subj_id, bs = "re"),
  #   data = df_scan_imp,
  #   family = gaussian(),
  #   method = "fREML",
  #   discrete = T
  # )
  # gam.check(fit_beh)
  # summary(fit_beh)
  # plot(fit_beh)
  # 
  # # Transform all
  # df_scan_imp$imp_tx <- NA
  # df_scan_imp$imp_tx <- log(df_scan_imp[, beh])
  # hist(df_scan_imp$imp_tx)
  # 
  # df_scan_imp$imp_tx <- df_scan_imp[, beh]/100
  # df_scan_imp$imp_tx <- df_scan_imp$imp_tx + 0.01
  # 
  # df_scan_imp$imp_tx <- df_scan_imp[, beh] + 0.5
  # 
  # hist(df_scan_imp$imp_tx, breaks=30)
  # descdist(df_scan_imp$imp_tx)
  # 
  # fit_tx <- bam(
  #   tot_symp ~ s(scan_count, bs = "tp", k = 3) +
  #     s(subj_id, bs = "re"),
  #   data = df_scan_imp,
  #   family = nb(),
  #   method = "fREML",
  #   discrete = T
  # )
  # gam.check(fit_tx)
  # summary(fit_tx)
  # plot(fit_tx)
  # 
  # fit_tx2 <- bam(
  #   imp_ctl ~ s(scan_count, bs = "tp", k = 3) +
  #     s(subj_id, bs = "re"),
  #   data = df_scan_imp,
  #   family = poisson(),
  #   method = "fREML",
  #   discrete = T
  # )
  # gam.check(fit_tx2)
  # summary(fit_tx2)
  # plot(fit_tx2)
  # 
  # 
  # library(itsadug)
  # compareML(fit_beh, fit_tx)
  # compareML(fit_beh, fit_tx2)
  # compareML(fit_tx, fit_tx2)
}



#' Model all tracts for post-base and rtp-base differences (delta).
#'
#' Conduct longitudinal HGAM with all tracts and scan times so subject variance
#' is pooled across tracts and scans.
#'
#' @param df_afq Dataframe containing AFQ data.
#' @param make_plots Logical, whether to draw all tract grids.
#' @returns mgcv::bam fit object.
export("dwi_gam_delta_all")
dwi_gam_delta_all <- function(df_afq, make_plots = T) {
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
  if (make_plots == FALSE) {
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


#' Title.
#'
#' TODO
export("dwi_gam_delta_time")
dwi_gam_delta_time <- function(df_afq, tract) {
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
export("dwi_gam_long_tract")
dwi_gam_long_tract <- function(df_afq, tract) {
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
export("dwi_gam_long_impact")
dwi_gam_long_impact <- function(
    df_afq, df_scan_imp, tract, impact_meas = "planned") {
  # Manage pre-planned or user-specified impact measure
  if (impact_meas == "planned") {
    impact_meas <- .tract_impact(as.character(tract))
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
