

# TODO survival analysis: time to an event

#' Identify subjects who got worse or better for vist
#' 
#' @param col_name Column name for testing.
#' @param df Dataframe of data.
#' @param visit_name Name of visit.
#' @return List with following elements:
#' @better element1 Subjects who got better
#' @worse element2 Subjects who got worse
better_worse <- function(col_name, df, visit_name){
  # Identify subjs who got better/worse
  fu1_bet <- df[
    which(df$fu1_change == "better" & df$visit_name == visit_name), 
    col_name
  ]
  fu1_wor <- df[
    which(df$fu1_change == "worse" & df$visit_name == visit_name), 
    col_name
  ]
  return(
    list(
      better=fu1_bet,
      worse=fu1_wor
    )
  )
}


#' Find average change
#' 
#' @param TODO
score_change <- function(col_name, df, visit_name){
  df_sub <- reshape(
    df, 
    idvar=c("subj_id", "fu1_change"), 
    timevar="visit_name", 
    direction = "wide"
  )
  
  base <- paste0(col_name, ".base")
  fu1 <- paste0(col_name, ".fu1")
  fu2 <- paste0(col_name, ".fu2")
  fu3 <- paste0(col_name, ".fu3")
  fu4 <- paste0(col_name, ".fu4")
  
  df_sub$d_fu1 <- df_sub[, fu1] - df_sub[, base]
  df_sub$d_fu2 <- df_sub[, fu2] - df_sub[, base]
  df_sub$d_fu3 <- df_sub[, fu3] - df_sub[, base]
  df_sub$d_fu4 <- df_sub[, fu4] - df_sub[, base]
  
  df_sub <- df_sub[, -c(3:7)]
  df_long <- reshape(
    df_sub,
    direction = "long",
    varying = c("d_fu1","d_fu2","d_fu3","d_fu4"),
    v.names = "value",
    timevar="visit_change",
    times = c("d_fu1", "d_fu2", "d_fu3", "d_fu4"),
    idvar=c("subj_id")
  )
  
  visit <- paste0("d_", visit_name)
  avg_bet <- round(
    mean(
      df_long[
        which(df_long$fu1_change == "better" & df_long$visit_change == visit),
      ]$value,
      na.rm = T
    ),
    digits=2
  )
  
  avg_wor <- round(
    mean(
      df_long[
        which(df_long$fu1_change == "worse" & df_long$visit_change == visit),
      ]$value,
      na.rm = T
    ),
    digits=2
  )
  
  return(
    list(
      avg_bet=avg_bet,
      avg_wor=avg_wor
    )
  )
}


#' Conduct Wilcoxon rank sum test.
#' 
#' @param col_name Column name for testing.
#' @param df Dataframe of data.
#' @param visit_name Name of visit.
#' @return List with following elements:
#' @stats element1 Output of wilcoxong test
#' @num_bet element2 Number of subjects who get better
#' @num_wor element3 Number of subjects who get worse 
wc_ranksum <- function(col_name, df, visit_name){
  bet_wor <- better_worse(col_name, df, visit_name)
  stats.wc <- wilcox.test(bet_wor$better, bet_wor$worse)
  return(
    list(
      stats = stats.wc, 
      num_bet = length(bet_wor$better), 
      num_wor = length(bet_wor$worse)
    )
  )
}