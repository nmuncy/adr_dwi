#' Draw Impact pairs accounting for kmeans clusters.
#'
#' @param df Dataframe with clustering info, workflows$impact_cluster$df_sik.
#' @param col_list List of relevant columns in df.
#' @param num_k Number of clusters.
export("draw_impact_pairs")
draw_impact_pairs <- function(df, col_list, num_k) {
  if (num_k == 3) {
    bg_arg <- c("blue", "red", "green")
  } else if (num_k == 5) {
    bg_arg <- c("blue", "red", "green", "orange", "purple")
  }
  plot_pairs <- pairs.panels(
    df[, col_list],
    bg = bg_arg[df$km_grp],
    gap = 0,
    pch = 21
  )
  # return(plot_pairs)
}


#' Draw K-means cluster.
#'
#' @param data_norm Dataframe of normalized data, from quick_stats$run_kmeans.
#' @param clust_km Object returned by stats::kmean.
#' @returns fviz_clister plot.
export("draw_kmeans")
draw_kmeans <- function(data_norm, clust_km) {
  plot_kmean <- fviz_cluster(list(data = data_norm, cluster = clust_km))
  return(plot_kmean)
}


#' Draw PCA eigenvector, biplots.
#'
#' @param stats_pc Object returned by stats::prcomp.
#' @returns Named list "plot_eig" = eigen plot, "plot_biplot = biplot.
export("draw_pca")
draw_pca <- function(stats_pc) {
  plot_eig <- fviz_eig(stats_pc, addlabels = T)
  plot_bip <- fviz_pca_biplot(stats_pc, label = "var")
  return(list("plot_eig" = plot_eig, "plot_biplot" = plot_bip))
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
export("visit_track")
visit_track <- function(col_name, df) {
  stats_base <- quick_stats$wc_ranksum(col_name, df, "base")
  stats_post <- quick_stats$wc_ranksum(col_name, df, "post")
  d_post <- quick_stats$score_change(col_name, df, "post")
  d_rtp <- quick_stats$score_change(col_name, df, "rtp")

  plot <- xyplot(
    get(col_name) ~ scan_name | base_v_post,
    data = df,
    group = subj_id,
    type = "b",
    ylab = col_name,
    main = paste0(
      "BetterVsWorse: p(base)=",
      round(stats_base$stats$p.value, 3),
      "; p(post)=",
      round(stats_post$stats$p.value, 3)
    ),
    sub = paste0(
      "Better n=", stats_base$num_bet,
      ", dpost=", d_post$avg_bet,
      ", drtp=", d_rtp$avg_bet,
      "; Worse n=", stats_base$num_wor,
      ", dpost=", d_post$avg_wor,
      ", drtp=", d_rtp$avg_wor
    )
  )
  return(plot)
}
