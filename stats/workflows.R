import(dplyr)
import(lubridate)
import("stats", "complete.cases")
import(mgcv)
import(fitdistrplus)
# import(itsadug)
import(mgcViz)
import(viridis)

pull_data <- use("resources/pull_data.R")
transform_data <- use("resources/transform_data.R")
fit_gams <- use("resources/fit_gams.R")
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
imp_bet_wor <- function(df) {
  for (col_name in c(
    "tot_symp", "mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl"
  )
  ) {
    low <- if (col_name %in% c("rx_time", "tot_symp")) T else F
    df_sub <- transform_data$compare_base_post(col_name, df, low = low)
    print(draw_plots$visit_track(col_name, df_sub))
  }
}


#' Fit DWI scalars with longitudinal HGAMs.
#' 
#' TODO
export("scalar_gams")
scalar_gams <- function(df_afq, tract, post_sess){
  if(! post_sess %in% c("post", "rtp")){
    stop("Unexpected post_sess")
  }
  #
  # df <- df_afq[which(
  #   df_afq$tract_name == tract &
  #     df_afq$scan_name %in% c("base", post_sess)
  # ), ]
  df <- df_afq[which(df_afq$tract_name == tract), ]
  
  #
  # hist(df$dti_md, breaks = 50)
  # plot(df$dti_md)
  # plot(df$node_id, df$dti_md)
  # plot(df$node_id, df$dti_md, col = factor(df$scan_name))
  
  #
  fit_FA <- fit_gams$gam_gs(df, "dti_fa")
  fit_FAO <- fit_gams$gam_gso(df, "dti_fa")
  plot_FA <- draw_plots$draw_grid(fit_FA, fit_FAO, 2, 3, 3, tract, "FA")
  
  fit_RD <- fit_gams$gam_gs(df, "dti_rd")
  fit_RDO <- fit_gams$gam_gso(df, "dti_rd")
  plot_RD <- draw_plots$draw_grid(fit_RD, fit_RDO, 2, 3, 3, tract, "RD")
  
  fit_AD <- fit_gams$gam_gs(df, "dti_ad")
  fit_ADO <- fit_gams$gam_gso(df, "dti_ad")
  plot_AD <- draw_plots$draw_grid(fit_AD, fit_ADO, 2, 3, 3, tract, "AD")
  
  fit_MD <- fit_gams$gam_gs(df, "dti_md")
  fit_MDO <- fit_gams$gam_gso(df, "dti_md")
  plot_MD <- draw_plots$draw_grid(fit_MD, fit_MDO, 2, 3, 3, tract, "MD")
  
  return(list(
    gam_GS = list(
      "FA" = fit_FA, "RD" = fit_RD, "AD" = fit_AD, "MD" = fit_MD
    ),
    gam_GSO = list(
      "FA" = fit_FAO, "RD" = fit_RDO, "AD" = fit_ADO, "MD" = fit_MDO
    ),
    gam_plots = list(
      "FA" = plot_FA, "RD" = plot_RD, "AD" = plot_AD, "MD" = plot_MD
    )
  ))
}


#' Fit DWI scalars X Impact with longitudinal HGAMs.
#' 
#' TODO
export("scalar_intx_gams")
scalar_intx_gams <- function(df_afq, df_scan_imp, tract, post_sess){
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


