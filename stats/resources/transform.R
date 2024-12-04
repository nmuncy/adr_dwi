import("tidyr")
import("tidyverse")
import("ggpubr")
import("stats", "reshape")


#' Identify subjects who get better from base to fu1.
#' 
#' @param col_name Column name for testing.
#' @param df Dataframe of data.
#' @param low Whether lower scores are better
#' @returns Dataframe with new column (fu1_change) indicating direction
#' of change.
export("fu1_better")
fu1_better <- function(col_name, df, low = FALSE){
  # Only first TBIs
  # TODO account for multiple TBIs
  df_sub <- df[
    which(! df$num_tbi %in% c(2:3)),
    c("subj_id", "visit_name", col_name)
  ]
  df_sub <- reshape(
    df_sub, idvar="subj_id", timevar="visit_name", direction = "wide"
  )
  
  # Drop subjs w/o fu1
  df_sub <- df_sub[!is.na(df_sub[paste0(col_name, ".base")]),]
  
  # Find subjs that get better from base-fu1
  df_sub$fu1_change <- "worse"
  if (low){
    df_sub$fu1_change[
      df_sub[paste0(col_name, ".fu1")] < df_sub[paste0(col_name, ".base")]
    ] = "better"
  } else {
    df_sub$fu1_change[
      df_sub[paste0(col_name, ".fu1")] > df_sub[paste0(col_name, ".base")]
    ] = "better"
  }
  
  df_sub$fu1_change <- as.factor(df_sub$fu1_change)
  
  # Convert back to long
  df_long <- reshape(
    df_sub,
    direction = "long",
    varying = c(
      paste(col_name, "base", sep="."),
      paste(col_name, "fu1", sep="."),
      paste(col_name, "fu2", sep="."),
      paste(col_name, "fu3", sep="."),
      paste(col_name, "fu4", sep=".")
    ),
    timevar = "visit_name",
    times = c("base", "fu1", "fu2", "fu3", "fu4"),
    idvar=c("subj_id")
  )
  df_long$visit_name <- as.factor(df_long$visit_name)
  return(df_long)
}

export("comp_group")
comp_group <- function(df){
  df_corr <- df[, 1:10]
  imp_names <- c("mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl", "tot_symp")
  colnames(df_corr)[5:10] <- imp_names
  
  #
  df_wide <- reshape(
    df_corr,
    idvar="subj_id",
    timevar="visit_name",
    direction = "wide"
  )
  
  # TODO refactor
  df_wide$mem_ver_chg_grp <- "worse"
  df_wide$mem_ver_chg_grp[
    df_wide$mem_ver.fu1 > df_wide$mem_ver.base
  ] = "better"
  df_wide$mem_ver_chg <- df_wide$mem_ver.fu1 - df_wide$mem_ver.base
  
  df_wide$tot_symp_chg_grp <- "worse"
  df_wide$tot_symp_chg_grp[
    df_wide$tot_symp.fu1 < df_wide$tot_symp.base
  ] = "better"
  df_wide$tot_symp_chg <- df_wide$tot_symp.fu1 - df_wide$tot_symp.base
  
  df_wide$mem_vis_chg_grp <- "worse"
  df_wide$mem_ver_chg_grp[
    df_wide$mem_vis.fu1 > df_wide$mem_vis.base
  ] = "better"
  df_wide$mem_vis_chg <- df_wide$mem_vis.fu1 - df_wide$mem_vis.base
  
  df_wide$vis_mot_chg_grp <- "worse"
  df_wide$mem_ver_chg_grp[
    df_wide$vis_mot.fu1 > df_wide$vis_mot.base
  ] = "better"
  df_wide$vis_mot_chg <- df_wide$vis_mot.fu1 - df_wide$vis_mot.base
  
  df_wide$rx_time_chg_grp <- "worse"
  df_wide$rx_time_chg_grp[
    df_wide$rx_time.fu1 < df_wide$rx_time.base
  ] = "better"
  df_wide$rx_time_chg <- df_wide$rx_time.fu1 - df_wide$rx_time.base
  
  df_wide$imp_ctl_chg_grp <- "worse"
  df_wide$imp_ctl_chg_grp[
    df_wide$imp_ctl.fu1 > df_wide$imp_ctl.base
  ] = "better"
  df_wide$imp_ctl_chg <- df_wide$imp_ctl.fu1 - df_wide$imp_ctl.base
  
  return(df_wide)
}

# Deprecated
.long_wide <- function(df){
  df_wide <- df %>%
    pivot_wider(
      names_from = c("visit_name", "num_tbi"),
      values_from = c("visit_date",
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