import(dplyr)
import(lubridate)
import("stats", "complete.cases")
import(mgcv)
import(itsadug)
import(mgcViz)
import(viridis)

pull_data <- use("resources/pull_data.R")
transform_data <- use("resources/transform_data.R")
draw_plots <- use("resources/draw_plots.R")


#' Pull and clean impact data.
#'
#' Update impact names and add days post FU1 (diff_post) for
#' and subsequent impact assessments.
#'
#' @returns Dataframe of impact composite scores.
.clean_impact <- function() {
  # Get data from db_adr
  df_imp <- pull_data$get_user_comp()

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
#' @return Dataframe of AFQ tract metrics.
export("clean_afq")
clean_afq <- function() {
  # Get data from db_adr
  df_afq <- pull_data$get_afq()

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
  return(df_scan_imp)
}


#' Generate impact better-worse data plots.
#' 
#' TODO
export("imp_bet_wor")
imp_bet_wor <- function(df){
  for(col_name in c(
    "tot_symp", "mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl")
  ){
    low <- if (col_name %in% c("rx_time", "tot_symp")) T else F
    df_sub <- transform_data$compare_base_post(col_name, df, low=low)
    print(draw_plots$visit_track(col_name, df_sub))
  }
}

#' HGAM of Callosum Superior Parietal.
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
    dti_fa ~ s(subj_id, scan_name, bs="re") +
      s(node_id, bs = "tp", k = 40, m=2) +
      s(node_id, scan_name, bs="fs", k = 40, m = 2),
    data = df,
    family = betar(link = "logit"),
    method = "fREML",
    discrete = T
  )
  gam.check(fit_S)
  summary(fit_S)
  plot(fit_S)

  #
  df$scanOF <- factor(df$scan_name, ordered = T)
  fit_SO <- bam(
    dti_fa ~ s(node_id, bs = "tp", k = 40, m = 2) +
      s(subj_id, bs = "re") +
      s(node_id, by = scanOF, bs = "fs", k = 40, m = 2),
    data = df,
    family = betar(link = "logit"),
    method = "fREML",
    discrete = T
  )
  # summary(fit_SO)
  # plot(fit_SO)
  return(list(gamGS = fit_S, gamGSO = fit_SO))
}


#' Draw smooth grid.
#'
#' TODO
export("draw_grid")
draw_grid <- function(fit_G, fit_GO, tract) {
  # Generate plots objs from smooths
  plot_G <- getViz(fit_G)
  pGlobal <- draw_plots$draw_global_smooth(plot_G, 1, tract)
  pGroup <- draw_plots$draw_group_smooth(plot_G, 3, tract)

  plot_GO <- getViz(fit_GO)
  pDiff <- draw_plots$draw_group_smooth_diff(plot_GO, 3, tract)

  # draw grid
  plot_list <- list(
    "global" = pGlobal,
    "group" = pGroup,
    "diff" = pDiff
  )
  name_list <- list(
    "col1" = paste(tract, "Smooths"),
    "rowL" = "Est. FA Fit",
    "rowR1" = "Global",
    "rowR2" = "Group",
    "rowR3" = "Difference",
    "bot1" = "Tract Node"
  )
  draw_plots$draw_one_three(plot_list, name_list, tract)
}
