import("tidyr")
import("tidyverse")
import("ggpubr")
import("stats", "reshape")


#' Change df based on subj_id and visit_name.x columns.
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


#' Identify subjects who get better from base to fu1.
#'
#' Deprecated.
#' 
#' TODO remove.
#'
#' @param col_name Column name for testing.
#' @param df Dataframe of data.
#' @param low Whether lower scores are better
#' @returns Dataframe with new column (fu1_change) indicating direction
#' of change.
export("fu1_better")
fu1_better <- function(col_name, df, low = FALSE) {
  # Only first TBIs
  # TODO account for multiple TBIs
  df_sub <- df[
    which(!df$num_tbi %in% c(2:3)),
    c("subj_id", "visit_name", col_name)
  ]
  df_sub <- reshape(
    df_sub,
    idvar = "subj_id", timevar = "visit_name", direction = "wide"
  )

  # Drop subjs w/o fu1
  df_sub <- df_sub[!is.na(df_sub[paste0(col_name, ".base")]), ]

  # Find subjs that get better from base-fu1
  df_sub$fu1_change <- "worse"
  if (low) {
    df_sub$fu1_change[
      df_sub[paste0(col_name, ".fu1")] < df_sub[paste0(col_name, ".base")]
    ] <- "better"
  } else {
    df_sub$fu1_change[
      df_sub[paste0(col_name, ".fu1")] > df_sub[paste0(col_name, ".base")]
    ] <- "better"
  }

  df_sub$fu1_change <- as.factor(df_sub$fu1_change)

  # Convert back to long
  df_long <- reshape(
    df_sub,
    direction = "long",
    varying = c(
      paste(col_name, "base", sep = "."),
      paste(col_name, "fu1", sep = "."),
      paste(col_name, "fu2", sep = "."),
      paste(col_name, "fu3", sep = "."),
      paste(col_name, "fu4", sep = ".")
    ),
    timevar = "visit_name",
    times = c("base", "fu1", "fu2", "fu3", "fu4"),
    idvar = c("subj_id")
  )
  df_long$visit_name <- as.factor(df_long$visit_name)
  return(df_long)
}


#' Determine where impact scores get better in post than base.
#'
#' @param col_name Column name for testing.
#' @param df Dataframe of data.
#' @param low Whether lower scores are better
#' @returns Dataframe with new column (fu1_change) indicating direction
#' of change.
export("compare_base_post")
compare_base_post <- function(col_name, df, low = FALSE) {
  # Widen data for easy columnar subtraction
  df <- subset(df, select = c("subj_id", "scan_name", col_name))
  df_wide <- reshape(
    df,
    idvar = "subj_id", timevar = "scan_name", direction = "wide"
  )

  # Find subjs that get better from base-post
  df_wide <- df_wide[!is.na(df_wide[paste0(col_name, ".post")]), ]
  df_wide$base_v_post <- "worse"
  if (low) {
    df_wide$base_v_post[
      df_wide[paste0(col_name, ".post")] < df_wide[paste0(col_name, ".base")]
    ] <- "better"
  } else {
    df_wide$base_v_post[
      df_wide[paste0(col_name, ".post")] > df_wide[paste0(col_name, ".base")]
    ] <- "better"
  }
  df_wide$base_v_post <- as.factor(df_wide$base_v_post)

  # Convert back to long
  df_long <- reshape(
    df_wide,
    direction = "long",
    varying = c(
      paste(col_name, "base", sep = "."),
      paste(col_name, "post", sep = "."),
      paste(col_name, "rtp", sep = ".")
    ),
    timevar = "scan_name",
    times = c("base", "post", "rtp"),
    idvar = c("subj_id")
  )
  df_long$scan_name <- as.factor(df_long$scan_name)
  return(df_long)
}


# Deprecated
# TODO remove
.long_wide <- function(df) {
  df_wide <- df %>%
    pivot_wider(
      names_from = c("visit_name", "num_tbi"),
      values_from = c(
        "visit_date",
        "userMemoryCompositeScoreVerbal",
        "userMemoryCompositeScoreVisual",
        "userVisualMotorCompositeScore",
        "userReactionTimeCompositeScore",
        "userImpulseControlCompositeScore",
        "userTotalSymptomScore",
        "userSymptom1",
        "userSymptom2",
        "userSymptom3",
        "userSymptom4",
        "userSymptom5",
        "userSymptom6",
        "userSymptom7",
        "userSymptom8",
        "userSymptom9",
        "userSymptom10",
        "userSymptom11",
        "userSymptom12",
        "userSymptom13",
        "userSymptom14",
        "userSymptom15",
        "userSymptom16",
        "userSymptom17",
        "userSymptom18",
        "userSymptom19",
        "userSymptom20",
        "userSymptom21",
        "userSymptom22"
      )
    )
  return(df_wide)
}
