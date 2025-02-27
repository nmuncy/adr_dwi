# Location for unused workflow methods. Offloaded here for cleaner main files.
# This contains both deprecated workflow methods as well as their stat 
# script call.


# Workflow methods ----

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



#' Run GAM with randomized sample
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


# Stat script call ----


# imp_smooth <- draw_plots$draw_impact_smooths(df_scan_imp)
# print(imp_smooth)


# summary(tract_gams$gam_LGIO$FA)
# grid::grid.newpage(); grid::grid.draw(tract_gams$gam_plots$FA)


# # Subject-level responses, cluster for post visit.
# workflows$impact_better_worse(df_scan_imp)
# imp_clust <- workflows$impact_cluster(df_scan_imp, "post")
# 
# # PCA stats and plots
# print(imp_clust$stats_pc)
# print(imp_clust$plot_pc$plot_eig)
# print(imp_clust$plot_pc$plot_biplot)
# 
# # K-means stats and plots
# print(imp_clust$stats_km)
# print(imp_clust$plot_km)
# draw_plots$draw_impact_pairs(imp_clust$df_sik, c(7:10), 3)



# # Identify thresholds for GAM sensitivity
# #
# # Sources of curvature variance are related to (at least) scan-rescan,
# # tractometry, and concussion.
# #
# # TODO Scan-rescan - use HCP data.
# # TODO Concussion - split base into two groups, look for group differences.
# 
# # Changes in FA resulting from rerunning pyAFQ on ADR base data
# df_afq_rr <- workflows$clean_afq("tbl_afq_rerun")
# fit_DI_rr <- workflows$gam_delta_rerun_all(df_afq, df_afq_rr)
# 
# # Changes in FA resulting from rescanning subj
# # Note - no longer using due to significant scan-rescan variance for
# # single subject. Using HCP data may be better, even if 
# # the data are from a different scanner and protocol.
# df_afq_rs <- workflows$clean_afq("tbl_afq_rescan")
# fit_LDI_rs <- workflows$gam_delta_rescan_all(df_afq_rs)
# 
# # Group differences by splitting base into two groups
# fit_I <- workflows$gam_fa_rebase_all(df_afq)
# 
# # Concatenate max and est dataframes
# workflows$plot_estimates(fit_LDI, fit_DI_rr, fit_LDI_rs)