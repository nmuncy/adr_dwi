# Set Env ----
library("lattice")
library("latticeExtra")
library("directlabels")
library("ggpubr")

source(paste0(getwd(), "/stats.R"))


meas_names <- function(meas) {
  # Switch variable name for something more readable.
  out_name <- switch(
    meas,
    "userMemoryCompositeScoreVerbal" = "Vebal Memory Comp",
    "userMemoryCompositeScoreVisual" = "Visual Memory Comp",
    "userVisualMotorCompositeScore" = "Visual Motor Comp",
    "userReactionTimeCompositeScore" = "Rx Time Comp",
    "userImpulseControlCompositeScore" = "Impulse Ctl Comp",
    "userTotalSymptomScore" = "Total Symptom",
  )
  return(out_name)
}


#' Make XYplot tracking participants by visit.
#' 
#' Adds Wilcoxon Rank-Sum testing to title for better and worse,
#' and identifies number of better worse and their corresponding
#' changes from baseline (for fu1, fu2).
#' 
#' @param col_name Column name for testing.
#' @param df Dataframe of data.
#' @param visit_name Name of visit.
#' @return Plot object.
visit_track <- function(col_name, df){
  stats_b <- wc_ranksum(col_name, df, "base")
  stats_f <- wc_ranksum(col_name, df, "fu1")
  d_fu1 <- score_change(col_name, df, "fu1")
  d_fu2 <- score_change(col_name, df, "fu2")
  
  plot <- xyplot(
    get(col_name) ~ visit_name | fu1_change, 
    data=df,
    group=subj_id,
    type="b",
    ylab = meas_names(col_name),
    main = paste0(
      "BetterVsWorse: p(base)=", 
      round(stats_b$stats$p.value, 3),
      "; p(fu1)=", 
      round(stats_f$stats$p.value, 3)
    ),
    sub = paste0(
      "Better n=", stats_b$num_bet, 
      ", dfu1=", d_fu1$avg_bet, 
      ", dfu2=", d_fu2$avg_bet, 
      "; Worse n=", stats_b$num_wor,
      ", dfu1=", d_fu1$avg_wor,
      ", dfu2=", d_fu2$avg_wor
    )
  )
  return(plot)
}



visit_box <- function(col_name, df){
  # Identify subjects who get better from base to fu1
  #
  # Arguments:
  #
  stats_b <- wc_ranksum(col_name, df, "base")
  stats_f <- wc_ranksum(col_name, df, "fu1")
  
  ggplot(
    df,
    aes(x=visit_name, y=get(col_name), fill=fu1_change),
  ) + 
    geom_boxplot() +
    ylab(meas_names(col_name)) +
    ggtitle(paste0(
      "base: bVw p=", 
      round(stats_b$p.value, 3),
      "; fu1: bVw p=", 
      round(stats_f$p.value, 3))
    )
}