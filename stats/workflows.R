import(dplyr)
import(lubridate)
import("stats", "complete.cases")

pull_data <- use("resources/pull_data.R")
transform_data <- use("resources/transform_data.R")

#' Pull and clean impact data.
#'
#' Update impact names and add days post FU1 (days_pfu1) for
#' and subsequent impact assessments.
#'
#' @returns Dataframe of impact composite scores.
.clean_impact <- function() {
  # Get data from db_adr
  df_imp <- pull_data$get_user_comp()

  # Manage column types, names
  df_imp$subj_id <- factor(df_imp$subj_id)
  df_imp$visit_name <- factor(df_imp$visit_name)
  df_imp$visit_date <- as.POSIXct(
    df_imp$visit_date,
    format = "%Y-%m-%d", tz = "UTC"
  )
  colnames(df_imp)[5:10] <- c(
    "mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl", "tot_symp"
  )

  # Calculate days since fu1
  idx_base <- which(df_imp$visit_name == "base")
  df_base <- df_imp[idx_base, ]
  df_post <- df_imp[-c(idx_base), ]
  rm(df_imp)
  df_post <- df_post %>%
    group_by(subj_id, num_tbi) %>%
    mutate(
      date = ymd(visit_date),
      days_pfu1 = as.numeric(date - min(date))
    )
  df_post <- subset(df_post, select = -c(date))
  # names(df_post)[names(df_post) == 'date'] <- 'visit_date'

  # Return combined dfs
  df_base$days_pfu1 <- NA
  df_comb <- rbind(df_base, df_post)
  rm(df_base, df_post)
  return(df_comb)
}

#' Pull and clean AFQ data.
#'
#' Clip tail nodes (x<10, x>89).
#'
#' @return Dataframe of AFQ tract metrics.
.clean_afq <- function() {
  # Get data from db_adr
  df_afq <- pull_data$get_afq()

  # Manage column types, clip tails
  df_afq$subj_id <- factor(df_afq$subj_id)
  df_afq$visit_name <- factor(df_afq$visit_name)
  df_afq$tract_name <- factor(df_afq$tract_name)
  df_afq$visit_date <- as.POSIXct(
    df_afq$visit_date,
    format = "%Y-%m-%d", tz = "UTC"
  )
  df_afq <- df_afq[which(df_afq$node_id > 9 & df_afq$node_id < 90), ]
  return(df_afq)
}

#' Minimize date distance between dfs A, B.
#'
#'
#' Identify which date in df B is closest to date
#' in df A, grouped by subj_id. Join df B to A.
#'
#' @param df_a Dataframe containing columns subj_id and visit_date.
#' @param df_b Tidy/long dataframe containing columns subj_id,
#' visit_date, and data.
#' @returns Dataframe df_b joined to df_a by minimal visit_date difference.
.min_time <- function(df_a, df_b) {
  df_b <- df_b %>% mutate(date = ymd(visit_date))
  df_a <- df_a %>% mutate(date = ymd(visit_date))

  df_a <- df_a %>%
    left_join(df_b, by = c("subj_id")) %>%
    mutate(time_diff = abs(date.x - date.y)) %>%
    group_by(subj_id, date.x) %>%
    arrange(time_diff) %>%
    slice(1) %>%
    ungroup()
  df_a <- subset(df_a, select = -c(date.x, date.y))
  return(df_a)
}


#' Get clean Impact and AFQ data.
#'
#' Identify relevant impact visit for scan,
#' add composites for scan.
#'
#' @returns Tidy dataframe of Impact and AFQ data.
export("get_afq_impact")
get_afq_impact <- function() {
  # Get cleaned data.
  df_imp <- .clean_impact()
  df_afq <- .clean_afq()

  # Match imp base to afq base. Merge dfs where visit names are base,
  # drop unmatching and irrelevant columns from df_imp.
  idx_imp_base <- which(df_imp$visit_name == "base")
  idx_afq_base <- which(df_afq$visit_name == "base")
  df_base <- merge(
    x = df_afq[idx_afq_base, ],
    y = df_imp[
      idx_imp_base,
      -which(names(df_imp) %in% c("visit_name", "num_tbi", "visit_date"))
    ],
    by = "subj_id",
    all.x = T
  )

  # Find closest imp fu for afq post, rtp
  df_imp_fu <- df_imp[
    -idx_imp_base,
    c("subj_id", "visit_name", "visit_date", "num_tbi")
  ]
  df_afq_post <- as.data.frame(unique(
    df_afq[
      which(df_afq$visit_name == "post"),
      c("subj_id", "visit_name", "visit_date")
    ]
  ))
  df_afq_post <- .min_time(df_afq_post, df_imp_fu)

  df_afq_rtp <- as.data.frame(unique(
    df_afq[
      which(df_afq$visit_name == "rtp"),
      c("subj_id", "visit_name", "visit_date")
    ]
  ))
  df_afq_rtp <- .min_time(df_afq_rtp, df_imp_fu)
  df_afq_post <- rbind(df_afq_post, df_afq_rtp)
  rm(df_afq_rtp)

  # Manage fringe cases, join impact metrics
  df_post_imp <- transform_data$fix_impact_scan(df_afq_post)
  rm(df_afq_post, df_imp_fu)
  df_post_imp <- subset(df_post_imp, select = -c(time_diff))
  df_post_imp <- df_post_imp[complete.cases(df_post_imp), ]
  colnames(df_post_imp)[2:5] <- c(
    "scan_name", "scan_date", "visit_name", "visit_date"
  )

  df_post_imp <- merge(
    x = df_post_imp,
    y = df_imp[
      ,
      c(
        "subj_id", "visit_name", "visit_date", "num_tbi",
        "mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl",
        "tot_symp", "days_pfu1"
      )
    ],
    by = c("subj_id", "visit_name", "visit_date", "num_tbi")
  )


  # Merge with afq data
  df_post_imp <- subset(
    df_post_imp,
    select = -c(visit_name, visit_date, num_tbi)
  )
  colnames(df_post_imp)[2:3] <- c("visit_name", "visit_date")
  df_post <- merge(
    x = df_afq[-idx_afq_base, ],
    y = df_post_imp,
    by = c("subj_id", "visit_name", "visit_date"),
    all.x = T
  )

  df_afq_imp <- rbind(df_base, df_post)
  rm(df_post, df_post_imp, df_base)
  return(df_afq_imp)
}
