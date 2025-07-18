#' Identify subjects who got worse or better for visit.
#'
#' @param col_name Column name for testing.
#' @param df Dataframe of data, output by workflows$get_scan_impact().
#' @param visit_name Name of visit.
#' @return List with following elements:
#'  'better' = element1 Subjects who got better
#'  'worse' = element2 Subjects who got worse
export("better_worse")
better_worse <- function(col_name, df, visit_name) {
  post_bet <- df[
    which(df$base_v_post == "better" & df$scan_name == visit_name),
    col_name
  ]
  post_wor <- df[
    which(df$base_v_post == "worse" & df$scan_name == visit_name),
    col_name
  ]
  return(
    list(
      better = post_bet,
      worse = post_wor
    )
  )
}



#' Find average change of metric.
#'
#' @param col_name Column name of df for testing.
#' @param df Dataframe of impact data.
#' @param visit_name String name of scan visit (base, post, or rtp).
#' @returns Named list, avg_bet = average value of those who got better
#' across visits; avg_wor = avergae value of those who got worse across visits.
export("score_change")
score_change <- function(col_name, df, visit_name) {
  # Cast wide and calculate differences
  df_sub <- reshape(
    df,
    idvar = c("subj_id", "base_v_post"),
    timevar = "scan_name",
    direction = "wide"
  )

  base <- paste0(col_name, ".base")
  post <- paste0(col_name, ".post")
  rtp <- paste0(col_name, ".rtp")

  df_sub$d_post <- df_sub[, post] - df_sub[, base]
  df_sub$d_rtp <- df_sub[, rtp] - df_sub[, base]

  # Cast long
  df_long <- reshape(
    df_sub,
    direction = "long",
    varying = c("d_post", "d_rtp"),
    v.names = "value",
    timevar = "visit_change",
    times = c("d_post", "d_rtp"),
    idvar = c("subj_id")
  )

  # Determine averages of scores that get better/worse across visits.
  visit <- paste0("d_", visit_name)
  avg_bet <- round(
    mean(
      df_long[
        which(df_long$base_v_post == "better" & df_long$visit_change == visit),
      ]$value,
      na.rm = T
    ),
    digits = 2
  )

  avg_wor <- round(
    mean(
      df_long[
        which(df_long$base_v_post == "worse" & df_long$visit_change == visit),
      ]$value,
      na.rm = T
    ),
    digits = 2
  )

  return(
    list(
      avg_bet = avg_bet,
      avg_wor = avg_wor
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
export("wc_ranksum")
wc_ranksum <- function(col_name, df, visit_name) {
  bet_wor <- better_worse(col_name, df, visit_name)
  stats.wc <- stats::wilcox.test(bet_wor$better, bet_wor$worse)
  return(
    list(
      stats = stats.wc,
      num_bet = length(bet_wor$better),
      num_wor = length(bet_wor$worse)
    )
  )
}



#' Conduct Principal Components Analysis of impact data.
#'
#' @param df Dataframe of impact scores.
#' @param col_list List of columns to include in PCA.
#' @returns PCA (prcomp) stats object.
export("run_pca")
run_pca <- function(df, col_list) {
  df <- df[stats::complete.cases(df[, col_list]), ]
  stats_pc <- stats::prcomp(df[, col_list], center = T, scale. = T)
  return(stats_pc)
}


#' Conduct K-means clustering
#'
#' @param df Dataframe of impact scores.
#' @param col_list List of columns to include in k-means analysis.
#' @param num_k Numeric, specify number of centers.
#' @returns Named list containing scaled data (data_norm), kmeans stats
#'    object (stats_km), and cluster lists (clust_km).
export("run_kmeans")
run_kmeans <- function(df, col_list, num_k) {
  df <- df[stats::complete.cases(df[, col_list]), ]
  data_norm <- scale(df[, col_list])
  stats_km <- stats::kmeans(data_norm, centers = num_k, nstart = 100)

  clust_km <- stats_km$cluster
  rownames(data_norm) <- df$subj_id
  return(list(
    "data_norm" = data_norm, "stats_km" = stats_km, "clust_km" = clust_km
  ))
}