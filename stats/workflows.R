import(grDevices)
import(dplyr)
import(lubridate)
import("stats", "complete.cases")

pull_data <- use("resources/pull_data.R")
transform_data <- use("resources/transform_data.R")
fit_gams <- use("resources/fit_gams.R")
draw_plots <- use("resources/draw_plots.R")
quick_stats <- use("resources/quick_stats.R")


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
#' @returns Dataframe of AFQ tract metrics.
export("clean_afq")
clean_afq <- function() {
  # Check for local csv, ead-in data or pull for db_adr.
  afq_path <- paste(.analysis_dir(), "dataframes", "df_afq.csv", sep = "/")

  if (!file.exists(afq_path)) {
    df <- pull_data$get_afq()
    utils::write.csv(df, afq_path, row.names = F)
    rm(df)
  }
  df_afq <- utils::read.csv(afq_path)

  # Manage column types, clip tails
  df_afq$subj_id <- factor(df_afq$subj_id)
  df_afq$scan_name <- factor(df_afq$scan_name)
  df_afq$tract_name <- factor(df_afq$tract_name)
  df_afq$scan_date <- as.POSIXct(
    df_afq$scan_date,
    format = "%Y-%m-%d", tz = "UTC"
  )
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
  df_afq <- clean_afq()

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
  plot_pc <- draw_plots$draw_pc(stats_pc)

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


#' Model all tracts for post-base and rtp-base differences (delta).
#'
#' Conduct longitudinal HGAM with all tracts and scan times so subject variance
#' is pooled across tracts and scans.
#'
#' @param df_afq Dataframe containing AFQ data.
#' @returns mgcv::bam fit object.
export("gam_delta_long_all")
gam_delta_long_all <- function(df_afq) {
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
  
  # Mine, print summary stats
  sum_ldi <- paste0(analysis_dir, "/stats_gams/gam_summaries/fit_LDI_fa.txt")
  if(!file.exists(sum_ldi)){
    fit_gams$write_gam_stats(fit_LDI, sum_ldi)
  }
  
  # Generate plots
  
  return(fit_LDI)
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
  plots_LGI <- draw_plots$draw_long_grid(
    fit_LGI, tract, toupper(scalar)
  )
  plots_LGIO <- draw_plots$draw_long_ordered_grid(
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


#' Fit DWI scalars X Impact with longitudinal HGAMs.
#'
#' TODO
export("gams_long_intx")
gams_long_intx <- function(df_afq, df_scan_imp, tract, post_sess) {
  # if(! post_sess %in% c("post", "rtp")){
  #   stop("Unexpected post_sess")
  # }

  #
  # df <- df_afq[which(
  #   df_afq$tract_name == tract &
  #     df_afq$scan_name %in% c("base", post_sess)
  # ), ]
  df <- df_afq[which(df_afq$tract_name == tract), ]

  #
  impact_meas <- "mem_vis"
  df <- merge(
    x = df,
    y = df_scan_imp[, c("subj_id", "scan_name", impact_meas)],
    by = c("subj_id", "scan_name"),
    all.x = T
  )

  #
  fit_FA <- fit_gams$gam_gs_intx(df, "dti_fa", impact_meas)
  fit_FAO <- fit_gams$gam_gso_intx(df, "dti_fa", impact_meas)


  return(list(
    gam_GS_intx = list(
      "FA" = fit_FA,
    ),
    gam_GSO_intx = list(
      "FA" = fit_FAO,
    ),
    gam_plots = list(
      "FA" = plot_FA,
    )
  ))
}


# TODO add methods below to workflow method, and refactor so that
# common methods are being used for all model fits.

#' #' HGAM of Callosum Superior Parietal.
#' #'
#' #' Fit HGAMs with AFQ FA values to generate global, group,
#' #' and ordered group smooths.
#' #'
#' #' @param df TODO
#' export("gam_spar")
#' gam_spar <- function(df) {
#'   #
#'   # fit_S <- bam(
#'   #   dti_fa ~ s(node_id, bs="tp", k=40, m=2) +
#'   #     s(subj_id, bs="re") +
#'   #     s(node_id, scan_name, bs="fs", k=40, m=2),
#'   #   data = df,
#'   #   family = betar(link="logit"),
#'   #   method = "fREML",
#'   #   discrete = T
#'   # )
#'   # gam.check(fit_S, rep=1000)
#'   # summary(fit_S)
#'   # plot(fit_S)
#'
#'   #
#'   fit_S <- bam(
#'     dti_fa ~ s(subj_id, scan_name, bs = "re") +
#'       s(node_id, bs = "tp", k = 40) +
#'       s(node_id, scan_name, bs = "fs", k = 40),
#'     data = df,
#'     family = betar(link = "logit"),
#'     method = "fREML",
#'     discrete = T
#'   )
#'   # gam.check(fit_S)
#'   # summary(fit_S)
#'   # plot(fit_S)
#'
#'   #
#'   df$scanOF <- factor(df$scan_name, ordered = T)
#'   fit_SO <- bam(
#'     dti_fa ~ s(subj_id, scan_name, bs = "re") +
#'       s(node_id, bs = "tp", k = 40) +
#'       s(node_id, by = scanOF, bs = "fs", k = 40),
#'     data = df,
#'     family = betar(link = "logit"),
#'     method = "fREML",
#'     discrete = T
#'   )
#'   # summary(fit_SO)
#'   # plot(fit_SO)
#'   return(list(gamGS = fit_S, gamGSO = fit_SO))
#' }
#'
#'
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
#'
#'
#' #' Gam interaction
#' #'
#' export("gam_intx")
#' gam_intx <- function(df) {
#'   fit_intx <- bam(
#'     dti_fa ~ s(subj_id, scan_name, bs = "re") +
#'       s(node_id, bs = "tp", k = 40) +
#'       s(tot_symp, by = scan_name, bs = "tp", k = 5) +
#'       ti(node_id, tot_symp, by = scan_name, bs = c("tp", "tp"), k = c(50, 5)),
#'     data = df,
#'     family = betar(link = "logit"),
#'     method = "fREML",
#'     discrete = T
#'   )
#'   # gam.check(fit_intx)
#'   # summary(fit_intx)
#'   # plot(fit_intx)
#'
#'   df$scanOF <- factor(df$scan_name, ordered = T)
#'   fit_intxOF <- bam(
#'     dti_fa ~ s(subj_id, scan_name, bs = "re") +
#'       s(node_id, bs = "tp", k = 40) +
#'       s(tot_symp, by = scan_name, bs = "tp", k = 5) +
#'       ti(node_id, tot_symp, bs = c("tp", "tp"), k = c(50, 5)) +
#'       ti(
#'         node_id, tot_symp, by = scanOF, bs = c("tp", "tp"), k = c(50, 5), m = 2
#'       ),
#'     data = df,
#'     family = betar(link = "logit"),
#'     method = "fREML",
#'     discrete = T
#'   )
#'
#'   return(list(gamIntx = fit_intx, gamIntxOF = fit_intxOF))
#' }
