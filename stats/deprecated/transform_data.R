
#' Identify subjects who get better from base to fu1.
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