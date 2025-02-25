# Methods for drawing and building plots.
#
# Public functions described below. Generally, functions that draw
# individual plots are named draw_*, and functions that assemble plots into
# a single plot are named grid_*. Functions are also named for the type
# of GAM used (GS, GIOS, IOS, IS, LDI, DI, LGI, LGIO) where appropriate.
#
# draw_impact_smooths: Draw GAM smooths of ImPACT composites.
# draw_impact_pairs: Draw pairs of ImPACT scores accounting for k-means groups.
# draw_kmeans: Draw K-means clustering plot.
# draw_pca: Draw PCA biplot and eigenvector.
# draw_scalar_grid: Draw 2x2 grid of scalar smooths.
# draw_gios_diff: Draw group difference smooths in Y-range of global smooth.
# draw_gios_diff_sig: Draw group difference smooths in y-range of global 
#   smooth with significance boxes.
# draw_gs: Draw global smooth and CI.
# draw_is: Draw group smooths and CIs.
# draw_is_intx: Draw group interaction smooths.
# draw_ios_diff_sig: Draw group difference smooths with significance boxes.
# grid_di_comb: Combine DI smooths into single image.
# grid_ldi: Draw smooth grid for LDI models.
# grid_ldi_comb: Combine LDI smooths into single image.
# grid_lgi: Draw smooth grid for LGI models, containing global & group smooths.
# grid_lgi_intx: Draw smooth grid for LGI_intx models.
# grid_lgio_intx: Draw smooth grid for LGIO_intx models.
# grid_lgio: Draw smooth grid for LGIO models, containing global 
#   and difference smooths.
# visit_track: Draw ImPACT values, tracking subject across visits.


import(ggplot2)
import(ggpubr)
import(mgcViz)
import(viridis)
import(itsadug)
import(gridExtra)
import(lattice)
import(latticeExtra)
import(directlabels)
import(factoextra)
import(psych)
import(reshape2)

quick_stats <- use("resources/quick_stats.R")
fit_gams <- use("resources/fit_gams.R")


#' Provide switch for Impact measure names.
#'
#' @param meas String impact column name.
#' @returns String reduced impact name.
.meas_names <- function(meas) {
  out_name <- switch(meas,
    "userMemoryCompositeScoreVerbal" = "Vebal Memory Comp",
    "userMemoryCompositeScoreVisual" = "Visual Memory Comp",
    "userVisualMotorCompositeScore" = "Visual Motor Comp",
    "userReactionTimeCompositeScore" = "Rx Time Comp",
    "userImpulseControlCompositeScore" = "Impulse Ctl Comp",
    "userTotalSymptomScore" = "Total Symptom",
  )
  return(out_name)
}

#' Provide switch for shortened Impact measure names.
#'
#' @param meas String impact column name.
#' @returns String reduced impact name.
.meas_short_names <- function(meas) {
  out_name <- switch(meas,
    "mem_ver" = "Vebal Memory",
    "mem_vis" = "Visual Memory",
    "vis_mot" = "Visual Motor",
    "rx_time" = "Rx Time",
    "imp_ctl" = "Impulse Ctl",
    "tot_symp" = "Total Symptom",
  )
  return(out_name)
}


#' Draw smooths of Impact measures by scan time.
#'
#' @param df_scan_imp Dataframe, returned by workflows$get_scan_impact.
#' @return Plot object.
export("draw_impact_smooths")
draw_impact_smooths <- function(df_scan_imp) {
  # Get relevant columns, convert to long
  df_wide <- subset(
    df_scan_imp,
    select = c(
      "subj_id", "scan_name", "mem_ver", "mem_vis",
      "vis_mot", "rx_time", "imp_ctl", "tot_symp"
    )
  )
  df <- reshape2::melt(
    df_wide,
    id.vars = c("subj_id", "scan_name"),
    variable.name = "impact",
    value.name = "value"
  )
  df$impact <- factor(df$impact)

  # Make continuous x
  df$scan_time <- 1
  df[which(df$scan_name == "post"), ]$scan_time <- 2
  df[which(df$scan_name == "rtp"), ]$scan_time <- 3

  # Draw and return
  p <- ggplot(data = df, aes(x = scan_time, y = value)) +
    facet_wrap(vars(impact), ncol = 3, scales = "free") +
    geom_smooth() +
    scale_x_continuous(breaks = 1:3) +
    ggtitle("IMPACT Composite and Symptom Scores") +
    xlab("Scan Session") +
    ylab("Response Value") +
    theme(text = element_text(family = "Times New Roman"))
  return(p)
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


#' Extract data fit from plotable, add LB, UB for CIs.
#'
#' @param p getViz plotable object from plot(sm(p, x)).
#' @col_names Optional, list of desired column names.
#' @returns Dataframe with added lb, ub columns.
.add_lb_ub <- function(p, col_names = c("nodeID", "est", "ty", "se")) {
  p_data <- as.data.frame(p$data$fit)
  colnames(p_data) <- col_names
  p_data$lb <- as.numeric(p_data$est - (1.96 * p_data$se))
  p_data$ub <- as.numeric(p_data$est + (1.96 * p_data$se))
  return(p_data)
}


#' Draw global smooth with confidence intervals.
#'
#' @param plot_obj Plotable object returned by getViz.
#' @param g_num Attribute number of plot_obj containing desired smooth.
#' @param x_min Optional, x-axis range LB.
#' @param x_max Optional, x-axis range UB.
#' @returns Named list, "global" = ggplot object.
export("draw_gs")
draw_gs <- function(plot_obj, g_num, x_min = 10, x_max = 89) {
  # use plot to extract attribute of interest
  p <- plot(sm(plot_obj, g_num))
  p_data <- .add_lb_ub(p)

  # make, save ggplot
  pp <- ggplot(data = p_data, aes(x = .data$nodeID, y = .data$est)) +
    geom_line() +
    geom_ribbon(aes(ymin = .data$lb, ymax = .data$ub), alpha = 0.2) +
    scale_x_continuous(breaks = c(seq(x_min, x_max, by = 10), x_max)) +
    theme(
      text = element_text(family = "Times New Roman"),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  return(list("global" = pp))
}


#' Draw group smooths with confidence intervals.
#'
#' @param plot_obj Plotable object returned by getViz.
#' @param i_num Attribute number of plot_obj containing desired smooth.
#' @param x_min Optional, x-axis range LB.
#' @param x_max Optional, x-axis range UB.
#' @returns Named list, "group" = ggplot object.
export("draw_is")
draw_is <- function(plot_obj, i_num, x_min = 10, x_max = 89) {
  # use plot to extract attribute of interest
  p <- plot(sm(plot_obj, i_num))
  p_data <- .add_lb_ub(p, col_names = c("nodeID", "est", "ty", "Group"))

  # make, save ggplot
  pp <- ggplot(
    data = p_data,
    aes(x = .data$nodeID, y = .data$est, group = .data$Group)
  ) +
    geom_line(aes(color = .data$Group)) +
    scale_y_continuous() +
    scale_x_continuous(breaks = c(seq(x_min, x_max, by = 10), x_max)) +
    scale_color_discrete(name = "") +
    theme(
      text = element_text(family = "Times New Roman"),
      legend.position = c(0.88, 0.85),
      legend.text = element_text(size = 8),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  return(list("group" = pp))
}


#' Draw group difference smooths in y-range of global smooth.
#'
#' @param plot_obj Plotable object returned by getViz.
#' @param g_num Attribute number of plot_obj containing global smooth.
#' @param i_num Attribute number of plot_obj containing group smooth.
#' @param x_min Optional, x-axis range LB.
#' @param x_max Optional, x-axis range UB.
#' @returns Named list, "diff" = ggplot object.
export("draw_gios_diff")
draw_gios_diff <- function(plot_obj, g_num, i_num, x_min = 10, x_max = 89) {
  # Determine ymin/max from group smooth
  p <- plot(sm(plot_obj, g_num))
  p_data <- .add_lb_ub(p)
  gy_min <- min(p_data$est)
  gy_max <- max(p_data$est)

  # unpack difference smooth data
  p <- plot(sm(plot_obj, i_num)) +
    geom_hline(yintercept = 0)
  p_data <- .add_lb_ub(p)

  # Determine highest/lowest max/min across global and group smooths
  GY_max <- pmax(gy_max, max(p_data$est))
  GY_min <- pmin(gy_min, min(p_data$est))

  # Draw
  pp <- ggplot(data = p_data, aes(x = .data$nodeID, y = .data$est)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_ribbon(
      aes(ymin = .data$lb, ymax = .data$ub),
      alpha = 0.2
    ) +
    scale_x_continuous(breaks = c(seq(x_min, x_max, by = 10), x_max)) +
    coord_cartesian(ylim = c(GY_min, GY_max)) +
    theme(
      text = element_text(family = "Times New Roman"),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  return(list("diff" = pp))
}


#' Determine which nodes differ from zero.
#'
#' @param p_data Dataframe from plotable$fit.
#' @param sig_dir String, "less" or "more".
#' @param returns NULL or dataframe containing dims for rect drawing.
.find_sig_nodes <- function(p_data, sig_dir = "less") {
  # Find sig rows
  if (sig_dir == "less") {
    sig_rows <- which(p_data$est < 0 & p_data$ub < 0)
    y_start <- min(p_data$lb)
    y_end <- 0
  } else if (sig_dir == "more") {
    sig_rows <- which(p_data$est > 0 & p_data$lb > 0)
    y_start <- 0
    y_end <- max(p_data$ub)
  }
  if (length(sig_rows) == 0) {
    return(NULL)
  }

  # Remove single rows - where a single node is identified as significant.
  rm_rows <- c()
  c <- 1
  while (c < length(sig_rows)) {
    row <- sig_rows[c]
    if (c == 1 && sig_rows[c + 1] > row + 1) { # First row is single
      rm_rows <- c(rm_rows, row)
    } else if (c == length(sig_rows) && sig_rows[c - 1] < row - 1) { # Final row is single
      rm_rows <- c(rm_rows, row)
    } else if (c > 1 && sig_rows[c - 1] < row - 1 && sig_rows[c + 1] > row + 1) { # Middle row is single
      rm_rows <- c(rm_rows, row)
    }
    c <- c + 1
  }
  sig_rows <- sig_rows[!sig_rows %in% rm_rows]

  # Identify significant nodes
  sig_nodes <- p_data[sig_rows, ]$nodeID
  if (length(sig_nodes) == 0) {
    return(NULL)
  }

  # find start, end points of sig regions
  vec_start <- sig_nodes[1]
  vec_end <- vector()
  num_nodes <- length(sig_nodes)
  c <- 2
  while (c < num_nodes) {
    cc <- c + 1
    if (sig_nodes[cc] > sig_nodes[c] + 1) {
      vec_end <- append(vec_end, sig_nodes[c])
      vec_start <- append(vec_start, sig_nodes[cc])
    }
    c <- cc
  }
  vec_end <- append(vec_end, sig_nodes[num_nodes])

  # make df for drawing rectangles
  d_rect <- data.frame(
    x_start = vec_start,
    x_end = vec_end,
    y_start = rep(y_start, length(vec_start)),
    y_end = rep(y_end, length(vec_start))
  )
  d_rect$x_start <- d_rect$x_start
  d_rect$x_end <- d_rect$x_end
  return(d_rect)
}


#' Draw smooth and account for sig rects.
#'
#' @param p_data Dataframe from plotable$fit.
#' @param rect_less Dataframe containing info for <0 rect.
#' @param rect_more Dataframe containing info for >0 rect.
#' @param x_min x-axis range LB.
#' @param x_max x-axis range UB.
#' @param y_min y-axis range LB.
#' @param y_max y-axis range UB.
.draw_smooth_rects <- function(
    p_data, rect_less, rect_more, x_min, x_max, y_min, y_max) {
  if (is.data.frame(rect_less) & is.data.frame(rect_more)) { # up and down rects
    d_rect <- rbind(rect_less, rect_more)
  } else if (is.data.frame(rect_less)) { # down rects
    d_rect <- rect_less
  } else if (is.data.frame(rect_more)) { # up rects
    d_rect <- rect_more
  } else { # no rects
    pp <- ggplot(data = p_data, aes(x = .data$nodeID, y = .data$est)) +
      geom_hline(yintercept = 0) +
      geom_line() +
      geom_ribbon(
        aes(ymin = .data$lb, ymax = .data$ub),
        alpha = 0.2
      ) +
      scale_x_continuous(breaks = c(seq(x_min, x_max, by = 10), x_max)) +
      coord_cartesian(ylim = c(y_min, y_max)) +
      theme(
        text = element_text(family = "Times New Roman"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
      )
    return(pp)
  }

  # Draw with rects
  pp <- ggplot(data = p_data, aes(x = .data$nodeID, y = .data$est)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_ribbon(
      aes(ymin = .data$lb, ymax = .data$ub),
      alpha = 0.2
    ) +
    annotate(
      "rect",
      xmin = c(d_rect$x_start),
      xmax = c(d_rect$x_end),
      ymin = c(d_rect$y_start),
      ymax = c(d_rect$y_end),
      alpha = 0.2,
      fill = "red"
    ) +
    scale_x_continuous(breaks = c(seq(x_min, x_max, by = 10), x_max)) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    theme(
      text = element_text(family = "Times New Roman"),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  return(pp)
}


#' Draw group difference smooths in y-range of global smooth with significance.
#'
#' @param plot_obj Plotable object returned by getViz.
#' @param g_num Attribute number of plot_obj containing global smooth.
#' @param i_num Attribute number of plot_obj containing group smooth.
#' @param x_min Optional, x-axis range LB.
#' @param x_max Optional, x-axis range UB.
#' @returns Named list, "diff" = ggplot object.
export("draw_gios_diff_sig")
draw_gios_diff_sig <- function(plot_obj, g_num, i_num, x_min = 10, x_max = 89) {
  # Determine ymin/max from group smooth
  p <- plot(sm(plot_obj, g_num))
  p_data <- .add_lb_ub(p)
  gy_min <- min(p_data$est)
  gy_max <- max(p_data$est)

  # unpack difference smooth data
  p <- plot(sm(plot_obj, i_num)) +
    geom_hline(yintercept = 0)
  p_data <- .add_lb_ub(p)

  # Determine highest/lowest max/min across global and group smooths
  GY_max <- pmax(gy_max, max(p_data$est))
  GY_min <- pmin(gy_min, min(p_data$est))

  # Find nodes that differ, draw
  rect_less <- .find_sig_nodes(p_data, sig_dir = "less")
  rect_more <- .find_sig_nodes(p_data, sig_dir = "more")

  pp <- .draw_smooth_rects(
    p_data, rect_less, rect_more, x_min, x_max, GY_min, GY_max
  )
  return(list("diff" = pp))
}


#' Draw group difference smooths with significance.
#'
#' @param plot_obj Plotable object returned by getViz.
#' @param i_num Attribute number of plot_obj containing group smooth.
#' @param x_min Optional, x-axis range LB.
#' @param x_max Optional, x-axis range UB.
#' @returns Named list, "diff" = ggplot object.
export("draw_ios_diff_sig")
draw_ios_diff_sig <- function(plot_obj, i_num, x_min = 10, x_max = 89) {
  # Unpack difference smooth data
  p <- plot(sm(plot_obj, i_num)) +
    geom_hline(yintercept = 0)
  p_data <- .add_lb_ub(p)

  # Find nodes that differ
  rect_less <- .find_sig_nodes(p_data, sig_dir = "less")
  rect_more <- .find_sig_nodes(p_data, sig_dir = "more")

  # Account for SE in ymin/max
  max_y <- max(p_data$est)
  max_node <- which(p_data$est == max_y)
  GY_max <- max_y + p_data[max_node, ]$ub

  min_y <- min(p_data$est)
  min_node <- which(p_data$est == min_y)
  GY_min <- min_y + p_data[min_node, ]$lb

  # Draw
  pp <- .draw_smooth_rects(
    p_data, rect_less, rect_more, x_min, x_max, GY_min, GY_max
  )
  return(list("diff" = pp))
}


#' Draw group interaction smooths.
#'
#' @param plot_obj Plotable object returned by getViz.
#' @param i_num Attribute number of plot_obj containing desired smooth.
#' @param i_name Name of group for title.
#' @param imp_name Name of impact measure for y-axis label.
#' @param zmin_zmax Named list holding min, max Z-values.
#' @param x_min Optional, x-axis range LB.
#' @param x_max Optional, x-axis range UB.
#' @returns Named list, "group" = ggplot object.
export("draw_is_intx")
draw_is_intx <- function(
    plot_obj, i_num, i_name, imp_name, zmin_zmax,
    x_min = 10, x_max = 89) {
  # use plot to extract attribute of interest
  p <- plot(sm(plot_obj, i_num))
  p_data <- as.data.frame(p$data$fit)
  colnames(p_data) <- c("fit", "tfit", "mem_vis", "nodeID", "se")

  # make, save ggplot
  pp <- ggplot(
    data = p_data,
    aes(x = .data$nodeID, y = .data$mem_vis, z = .data$fit)
  ) +
    geom_tile(aes(fill = fit)) +
    geom_contour(colour = "black") +
    scale_x_continuous(breaks = c(seq(x_min, x_max, by = 10), x_max)) +
    scale_fill_viridis(
      option = "D",
      name = "Est. FA Fit",
      limits = c(zmin_zmax$zmin, zmin_zmax$zmax)
    ) +
    labs(
      title = paste("Visit:", i_name),
      y = imp_name,
    ) +
    theme(
      text = element_text(family = "Times New Roman"),
      legend.text = element_text(size = 8),
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_blank()
    )
  return(list("group" = pp))
}


#' Title.
#' 
#' TODO
export("draw_di_time")
draw_di_time <- function(plot_obj, tract, x_min = 10, x_max = 89){
  p <- plot(sm(plot_obj, 4))
  p_data <- as.data.frame(p$data$fit)
  colnames(p_data) <- c("fit", "tfit", "time", "nodeID", "se")
  
  pp <- ggplot(
    data = p_data,
    aes(x = .data$nodeID, y = .data$time, z = .data$fit)
  ) +
    geom_tile(aes(fill = fit)) +
    geom_contour(colour = "black") +
    scale_x_continuous(breaks = c(seq(x_min, x_max, by = 10), x_max)) +
    scale_fill_viridis(
      option = "D",
      name = "Est. FA Fit",
    ) +
    labs(
      title = paste("RTP-Post", tract, "FA Change"),
      y = "Days",
      x = "Tract Node"
    ) +
    theme(
      text = element_text(family = "Times New Roman"),
      legend.text = element_text(size = 8),
      plot.title = element_text(hjust = 0.5)
    )
  return(list("time_diff" = pp))
}


#' Assemble a 1x2 grid of plots.
#'
#' @param plot_list Named list with diffa, and diffb attrs, containing
#'    plotable objects.
#' @param name_list Named list with col1, bot1, and rowL1:2 attrs, containing
#'    column, bottom, and left row strings.
#' @returns Object returned by grid.arrange.
.arr_one_two <- function(plot_list, name_list) {
  # unpack, organize plots
  r1A <- plot_list$Ia$diff
  r2A <- plot_list$Ib$diff

  # make col titles, y axis, x axis, and row names
  col1_name <- text_grob(name_list$col1, size = 12, family = "Times New Roman")
  bot1_name <- text_grob(name_list$bot1, size = 10, family = "Times New Roman")

  l1_name <-
    text_grob(name_list$rowL1, size = 12, family = "Times New Roman", rot = 90)
  l2_name <-
    text_grob(name_list$rowL2, size = 12, family = "Times New Roman", rot = 90)

  pOut <- grid.arrange(
    arrangeGrob(r1A, top = col1_name, left = l1_name),
    arrangeGrob(r2A, bottom = bot1_name, left = l2_name),
    nrow = 2,
    ncol = 1,
    widths = 1,
    heights = c(1, 1)
  )
  return(pOut)
}


#' Assemble a 1x3 grid of plots.
#'
#' @param plot_list Named list with global, diffa, and diffb attrs, containing
#'    plotable objects.
#' @param name_list Named list with col1, bot1, rowL1:3 and rowR1:3 attrs,
#'    containing column, bottom, and left/right row strings.
#' @returns Object returned by grid.arrange.
.arr_one_three <- function(plot_list, name_list) {
  # unpack, organize plots
  r1A <- plot_list$global$global
  r2A <- plot_list$diffa$diff
  r3A <- plot_list$diffb$diff

  # make col titles, y axis, x axis, and row names
  col1_name <- text_grob(name_list$col1, size = 12, family = "Times New Roman")
  bot1_name <- text_grob(name_list$bot1, size = 10, family = "Times New Roman")

  l1_name <-
    text_grob(name_list$rowL1, size = 12, family = "Times New Roman", rot = 90)
  l2_name <-
    text_grob(name_list$rowL2, size = 12, family = "Times New Roman", rot = 90)
  l3_name <-
    text_grob(name_list$rowL3, size = 12, family = "Times New Roman", rot = 90)

  r1_name <- text_grob(
    name_list$rowR1,
    size = 12, family = "Times New Roman", rot = 270
  )
  r2_name <- text_grob(
    name_list$rowR2,
    size = 12, family = "Times New Roman", rot = 270
  )
  r3_name <- text_grob(
    name_list$rowR3,
    size = 12, family = "Times New Roman", rot = 270
  )

  pOut <- grid.arrange(
    arrangeGrob(r1A, top = col1_name, left = l1_name),
    arrangeGrob(r2A, left = l2_name),
    arrangeGrob(r3A, bottom = bot1_name, left = l3_name),
    nrow = 3,
    ncol = 1,
    widths = 1,
    heights = c(1, 1, 1)
  )
  return(pOut)
}


#' Assemble a 1x4 grid of plots.
#'
#' @param plot_list Named list with global, Ia, Ib, and Ic attrs, containing
#'    plotable objects.
#' @param name_list Named list with col1, bot1, rowL1:4 and rowR1:4 attrs,
#'    containing column, bottom, and left/right row strings.
#' @returns Object returned by grid.arrange.
.arr_one_four <- function(plot_list, name_list) {
  # unpack, organize plots
  r1A <- plot_list$global$global
  r2A <- plot_list$Ia$diff
  r3A <- plot_list$Ib$diff
  r4A <- plot_list$Ic$diff

  # make col titles, y axis, x axis, and row names
  col1_name <- text_grob(name_list$col1, size = 12, family = "Times New Roman")
  bot1_name <- text_grob(name_list$bot1, size = 10, family = "Times New Roman")

  l1_name <-
    text_grob(name_list$rowL1, size = 12, family = "Times New Roman", rot = 90)
  l2_name <-
    text_grob(name_list$rowL2, size = 12, family = "Times New Roman", rot = 90)
  l3_name <-
    text_grob(name_list$rowL3, size = 12, family = "Times New Roman", rot = 90)
  l4_name <-
    text_grob(name_list$rowL4, size = 12, family = "Times New Roman", rot = 90)

  r1_name <- text_grob(
    name_list$rowR1,
    size = 12, family = "Times New Roman", rot = 270
  )
  r2_name <- text_grob(
    name_list$rowR2,
    size = 12, family = "Times New Roman", rot = 270
  )
  r3_name <- text_grob(
    name_list$rowR3,
    size = 12, family = "Times New Roman", rot = 270
  )
  r4_name <- text_grob(
    name_list$rowR4,
    size = 12, family = "Times New Roman", rot = 270
  )

  pOut <- grid.arrange(
    arrangeGrob(r1A, top = col1_name, left = l1_name, right = r1_name),
    arrangeGrob(r2A, left = l2_name, right = r2_name),
    arrangeGrob(r3A, left = l3_name, right = r3_name),
    arrangeGrob(r4A, bottom = bot1_name, left = l4_name, right = r4_name),
    nrow = 4,
    ncol = 1,
    widths = 1,
    heights = c(1, 1, 1, 1)
  )
  return(pOut)
}


#' Assemble a 2x3 grid of plots.
#'
#' Deprecated.
#'
#' @param plot_list Named list with beh, intx, and intx_diff attrs, containing
#'    plotable objects.
#' @param name_list Named list with col1:2, bot1:2, rowL and rowR1:3 attrs,
#'    containing column, bottom, and left/right row strings.
.arr_two_three <- function(plot_list, name_list) {
  # unpack, organize plots
  r1A <- plot_list$beh$con
  r1B <- plot_list$intx$con
  r2A <- plot_list$beh$exp
  r2B <- plot_list$intx$exp
  r3A <- plot_list$beh$diff
  r3B <- plot_list$intx_diff$diff

  # make col titles, y axis, x axis, and row names
  col1_name <- text_grob(name_list$col1, size = 12, family = "Times New Roman")
  col2_name <- text_grob(name_list$col2, size = 12, family = "Times New Roman")
  bot1_name <- text_grob(name_list$bot1, size = 10, family = "Times New Roman")
  bot2_name <- text_grob(name_list$bot2, size = 10, family = "Times New Roman")

  l1_name <- l3_name <-
    text_grob("", size = 12, family = "Times New Roman", rot = 90)
  l2_name <-
    text_grob(name_list$rowL, size = 12, family = "Times New Roman", rot = 90)

  r1_name <- text_grob(
    name_list$rowR1,
    size = 12, family = "Times New Roman", rot = 270
  )
  r2_name <- text_grob(
    name_list$rowR2,
    size = 12, family = "Times New Roman", rot = 270
  )
  r3_name <- text_grob(
    name_list$rowR3,
    size = 12, family = "Times New Roman", rot = 270
  )

  pOut <- grid.arrange(
    arrangeGrob(r1A, top = col1_name, left = l1_name),
    arrangeGrob(r1B, top = col2_name, right = r1_name),
    arrangeGrob(r2A, left = l2_name),
    arrangeGrob(r2B, right = r2_name),
    arrangeGrob(r3A, bottom = bot1_name, left = l3_name),
    arrangeGrob(r3B, bottom = bot2_name, right = r3_name),
    nrow = 3,
    ncol = 2,
    widths = c(0.75, 1),
    heights = c(1, 0.8, 1)
  )
  print(pOut)
}


#' Draw smooth grid for LDI models.
#'
#' @param fit_LDI mgcv::gam object.
#' @param tract String tract name for title.
#' @param scalar_name String dwi metric for title.
#' @param num_Ia Number attribute of fit_LDI holding group A smooth.
#' @param num_Ib Number attribute of fit_LDI holding group B smooth.
#' @returns Object returned by grid.arrange.
export("grid_ldi")
grid_ldi <- function(fit_LDI, tract, scalar_name, num_Ia, num_Ib) {
  # Generate plots objs from smooths
  p <- getViz(fit_LDI)
  pIa <- draw_ios_diff_sig(p, num_Ia)
  pIb <- draw_ios_diff_sig(p, num_Ib)

  # draw grid
  plot_list <- list(
    "Ia" = pIa,
    "Ib" = pIb
  )
  name_list <- list(
    "col1" = paste("LDI:", tract, scalar_name, "Smooths"),
    "rowL1" = "Diff: Post-Base",
    "rowL2" = "Diff: RTP-Base",
    "bot1" = "Tract Node"
  )
  .arr_one_two(plot_list, name_list)
}


#' Determine y-min and y-max from plottable.
#' 
#' @param plot_obj mgcv::getViz object.
#' @param num_list List of plottable indices.
#' @returns list of ymin, ymax.
.get_ymin_ymax <- function(plot_obj, num_list) {
  # Start with data from first plottable
  c <- 1
  p <- plot(sm(plot_obj, num_list[c]))
  p_data <- as.data.frame(p$data$fit)

  # Get data from rest of plottables
  c <- c + 1
  while (c <= as.numeric(length(num_list))) {
    p <- plot(sm(plot_obj, num_list[c]))
    h_data <- as.data.frame(p$data$fit)
    p_data <- rbind(p_data, h_data)
    c <- c + 1
  }
  return(list(ymin = min(p_data$y, na.rm = T), ymax = max(p_data$y, na.rm = T)))
}


#' Draw combined LDI plot.
#' 
#' @param plot_obj mgcv::getViz object.
#' @param idx_list List of plottable indices.
#' @param smooth_list List of smooth indices.
#' @param name_list List of names for smooth_list.
#' @param group Name of grouping factor (e.g. 'Left' for LH tracts).
#' @param y_min Numeric y-minimum.
#' @param y_max Numeric y-maximum.
#' @param x_min Numeric x-minimum.
#' @param x_max Numeric x-max.
#' @param add_bot Optional bool, add axis label to bottom.
#' @param add_top Optional bool, add axis label to top.
#' @returns Plottable object.
.draw_ldi_comb <- function(
    plot_obj, idx_list, smooth_list, name_list, group, 
    y_min, y_max, x_min, x_max, add_bot = F, add_top = F
){
  # Start dataframe for first index in idx_list
  num <- idx_list[1]
  p <- plot(sm(plot_obj, smooth_list[num]))
  p_data <- as.data.frame(p$data$fit)
  p_data$tract <- fit_gams$switch_tract(name_list[num])
  
  # Aggregate dataframe with remaining indices
  for (num in idx_list[2:length(idx_list)]) {
    p <- plot(sm(plot_obj, smooth_list[num]))
    h_data <- as.data.frame(p$data$fit)
    h_data$tract <- fit_gams$switch_tract(name_list[num])
    p_data <- rbind(p_data, h_data)
  }
  
  # Build plot
  p <- ggplot(
    data = p_data,
    aes(x = .data$x, y = .data$y, group = .data$tract)
  ) +
    geom_line(aes(color = .data$tract)) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    scale_x_continuous(breaks = c(seq(x_min, x_max, by = 10), x_max)) +
    scale_color_discrete(name = "") +
    theme(
      text = element_text(family = "Times New Roman"),
      legend.text = element_text(size = 10)
    )
    
  # Account for user options.
  if(add_top){
    p <- p + 
      ggtitle(paste(group, "FA Smooths")) +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  }else{
    p <- p + theme(
      plot.title = element_blank()
    )
  }
  
  if(add_bot){
    p <- p + 
      xlab("Node ID") +
      theme(
        axis.title.y = element_blank()
      )
  }else{
    p <- p + theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
    )
  }
  return(p)
}


#' Combine LDI smooths into single image.
#'
#' @param fit_LDI mgcv::gam object.
#' @param scalar_name Optional, string DWI metric for title.
#' @returns Object returned by grid.arrange.
export("grid_ldi_comb")
grid_ldi_comb <- function(
    fit_LDI, post_smooths, rtp_smooths, name_smooths,
    scalar_name = "FA", x_min = 10, x_max = 89, 
    comp_a = "Post-Base", comp_b = "RTP-Base") {
  # Generate plots objs from smooths
  plot_obj <- getViz(fit_LDI)

  # Identify CC, L, R smooth indices
  idx_cc <- match(name_smooths[grepl("Callosum", name_smooths)], name_smooths)
  idx_left <- match(name_smooths[grepl("Left", name_smooths)], name_smooths)
  idx_right <- match(name_smooths[grepl("Right", name_smooths)], name_smooths)

  # Build post plots
  ymin_ymax <- .get_ymin_ymax(plot_obj, post_smooths)
  p_post_cc <- .draw_ldi_comb(
    plot_obj, idx_cc, post_smooths, name_smooths, "Callosum", 
    ymin_ymax$ymin, ymin_ymax$ymax, x_min, x_max, add_top=T
  )
  p_post_left <- .draw_ldi_comb(
    plot_obj, idx_left, post_smooths, name_smooths, "Left", 
    ymin_ymax$ymin, ymin_ymax$ymax, x_min, x_max, add_top=T
  )
  p_post_right <- .draw_ldi_comb(
    plot_obj, idx_right, post_smooths, name_smooths, "Right", 
    ymin_ymax$ymin, ymin_ymax$ymax, x_min, x_max, add_top=T
  )
  
  # Build rtp plots
  ymin_ymax <- .get_ymin_ymax(plot_obj, rtp_smooths)
  p_rtp_cc <- .draw_ldi_comb(
    plot_obj, idx_cc, rtp_smooths, name_smooths, "Callosum", 
    ymin_ymax$ymin, ymin_ymax$ymax, x_min, x_max, add_bot=T
  )
  p_rtp_left <- .draw_ldi_comb(
    plot_obj, idx_left, rtp_smooths, name_smooths, "Left", 
    ymin_ymax$ymin, ymin_ymax$ymax, x_min, x_max, add_bot=T
  )
  p_rtp_right <- .draw_ldi_comb(
    plot_obj, idx_right, rtp_smooths, name_smooths, "Right", 
    ymin_ymax$ymin, ymin_ymax$ymax, x_min, x_max, add_bot=T
  )
  
  # draw grid
  left1_name <- text_grob(
    paste("Diff:", comp_a), size = 12, family = "Times New Roman", rot = 90
  )
  left2_name <- text_grob(
    paste("Diff:", comp_b), size = 12, family = "Times New Roman", rot = 90
  )
  plot_grid <- grid.arrange(
    arrangeGrob(p_post_cc, left = left1_name),
    arrangeGrob(p_post_left),
    arrangeGrob(p_post_right),
    arrangeGrob(p_rtp_cc, left = left2_name),
    arrangeGrob(p_rtp_left),
    arrangeGrob(p_rtp_right),
    nrow = 2,
    ncol = 3,
    widths = c(1, 1, 1),
    heights = c(1, 1)
  )
  return(plot_grid)
}


#' Combine DI smooths into single image.
#'
#' @param fit_LDI mgcv::gam object.
#' @param scalar_name Optional, string DWI metric for title.
#' @returns Object returned by grid.arrange.
export("grid_di_comb")
grid_di_comb <- function(
    fit_DI, node_smooths, name_smooths, 
    scalar_name = "FA", x_min = 10, x_max = 89) {
  # Generate plots objs from smooths
  plot_obj <- getViz(fit_DI)

  # Identify CC, L, R smooth indices
  idx_cc <- match(name_smooths[grepl("Callosum", name_smooths)], name_smooths)
  idx_left <- match(name_smooths[grepl("Left", name_smooths)], name_smooths)
  idx_right <- match(name_smooths[grepl("Right", name_smooths)], name_smooths)
  
  # Build post plots
  ymin_ymax <- .get_ymin_ymax(plot_obj, node_smooths)
  p_cc <- .draw_ldi_comb(
    plot_obj, idx_cc, node_smooths, name_smooths, "Callosum", 
    ymin_ymax$ymin, ymin_ymax$ymax, x_min, x_max, add_top=T, add_bot=T
  )
  p_left <- .draw_ldi_comb(
    plot_obj, idx_left, node_smooths, name_smooths, "Left", 
    ymin_ymax$ymin, ymin_ymax$ymax, x_min, x_max, add_top=T, add_bot=T
  )
  p_right <- .draw_ldi_comb(
    plot_obj, idx_right, node_smooths, name_smooths, "Right", 
    ymin_ymax$ymin, ymin_ymax$ymax, x_min, x_max, add_top=T, add_bot=T
  )
  
  # draw grid
  left1_name <- text_grob(
    "Diff: Run-Rerun", size = 12, family = "Times New Roman", rot = 90
  )
  plot_grid <- grid.arrange(
    arrangeGrob(p_cc, left = left1_name),
    arrangeGrob(p_left),
    arrangeGrob(p_right),
    nrow = 1,
    ncol = 3,
    widths = c(1, 1, 1),
    heights = 1
  )
  return(plot_grid)
}


#' Draw smooth grid for LGI models, containing global and group smooths.
#'
#' @param fit_LGI mgcv::gam object.
#' @param tract String tract name for title.
#' @param scalar_name String dwi metric for title.
#' @param num_G Number attribute of fit_LGI holding global smooth.
#' @param num_Ia Number attribute of fit_LGI holding group A smooth.
#' @param num_Ib Number attribute of fit_LGI holding group B smooth.
#' @param num_Ic Number attribute of fit_LGI holding group C smooth.
#' @returns Object returned by grid.arrange.
export("grid_lgi")
grid_lgi <- function(
    fit_LGI, tract, scalar_name, num_G = 2, num_Ia = 3, num_Ib = 4, num_Ic = 5) {
  # Generate plots objs from smooths
  plot_GI <- getViz(fit_LGI)
  pGlobal <- draw_gs(plot_GI, num_G)
  pIa <- draw_gios_diff(plot_GI, num_G, num_Ia)
  pIb <- draw_gios_diff(plot_GI, num_G, num_Ib)
  pIc <- draw_gios_diff(plot_GI, num_G, num_Ic)

  # draw grid
  plot_list <- list(
    "global" = pGlobal,
    "Ia" = pIa,
    "Ib" = pIb,
    "Ic" = pIc
  )
  name_list <- list(
    "col1" = paste("LGI:", tract, scalar_name, "Smooths"),
    "rowL1" = "Est. Global Fit",
    "rowL2" = "Est. Base Fit",
    "rowL3" = "Est. Post Fit",
    "rowL4" = "Est. RTP Fit",
    "bot1" = "Tract Node"
  )
  plot_grid <- .arr_one_four(plot_list, name_list)
  return(plot_grid)
}


#' Identify Z-min and Z-max across multiple plots.
#'
#' @param plot_obj Plotable object returned by getViz.
#' @param num_Ia Number attribute of plot_obj holding group A smooth.
#' @param num_Ib Number attribute of plot_obj holding group B smooth.
#' @param num_Ic Number attribute of plot_obj holding group C smooth.
.get_zmin_zmax <- function(plot_obj, num_Ia = 6, num_Ib = 7, num_Ic = 8) {
  z_min_all <- z_max_all <- c()
  for (num in c(num_Ia, num_Ib, num_Ic)) {
    p <- plot(sm(plot_obj, num))
    p_data <- as.data.frame(p$data$fit)
    colnames(p_data) <- c("fit", "tfit", "cov", "nodeID", "se")
    z_min_all <- c(z_min_all, min(c(p_data$fit, p_data$fit), na.rm = T))
    z_max_all <- c(z_max_all, max(c(p_data$fit, p_data$fit), na.rm = T))
  }
  return(list(zmin = min(z_min_all), zmax = max(z_max_all)))
}


#' Draw smooth grid for LGI_intx models.
#'
#' @param fit_LGI mgcv::gam object.
#' @param tract String tract name for title.
#' @param scalar_name String dwi metric for title.
#' @param num_Ia Number attribute of fit_LGI holding group A smooth.
#' @param num_Ib Number attribute of fit_LGI holding group B smooth.
#' @param num_Ic Number attribute of fit_LGI holding group C smooth.
#' @returns Object returned by grid.arrange.
export("grid_lgi_intx")
grid_lgi_intx <- function(
    fit_LGI_intx, tract, scalar_name, impact_meas,
    num_Ia = 6, num_Ib = 7, num_Ic = 8) {
  # Generate plots objs from group interaction smooths
  plot_obj <- getViz(fit_LGI_intx)
  imp_name <- .meas_short_names(impact_meas)
  zmin_zmax <- .get_zmin_zmax(plot_obj)
  plot_base <- draw_is_intx(plot_obj, num_Ia, "Base", imp_name, zmin_zmax)
  plot_post <- draw_is_intx(plot_obj, num_Ib, "Post", imp_name, zmin_zmax)
  plot_rtp <- draw_is_intx(plot_obj, num_Ic, "RTP", imp_name, zmin_zmax)

  # draw grid
  col1_name <- text_grob(paste("IMPACT by", tract, scalar_name), size = 12, family = "Times New Roman")
  bot1_name <- text_grob("Tract Node", size = 12, family = "Times New Roman")
  plot_grid <- grid.arrange(
    arrangeGrob(plot_base$group, top = col1_name),
    arrangeGrob(plot_post$group),
    arrangeGrob(plot_rtp$group, bottom = bot1_name),
    nrow = 3,
    ncol = 1,
    widths = 1,
    heights = c(1, 1, 1)
  )
  return(plot_grid)
}


#' Draw smooth grid for LGIO_intx models.
#'
#' @param fit_LGIO mgcv::gam object.
#' @param tract String tract name for title.
#' @param scalar_name String dwi metric for title.
#' @param num_Ia Number attribute of fit_LGIO holding group A smooth.
#' @param num_Ib Number attribute of fit_LGIO holding group B smooth.
#' @returns Object returned by grid.arrange.
export("grid_lgio_intx")
grid_lgio_intx <- function(
    fit_LGIO_intx, tract, scalar_name, impact_meas, num_Ia = 7, num_Ib = 8) {
  # Generate plots objs from group interaction smooths
  plot_obj <- getViz(fit_LGIO_intx)
  imp_name <- .meas_short_names(impact_meas)
  zmin_zmax <- .get_zmin_zmax(plot_obj)
  plot_a <- draw_is_intx(
    plot_obj, num_Ia, "Post-Base", imp_name, zmin_zmax
  )
  plot_b <- draw_is_intx(
    plot_obj, num_Ib, "RTP-Base", imp_name, zmin_zmax
  )

  # draw grid
  col1_name <- text_grob(paste("IMPACT by", tract, scalar_name), size = 12, family = "Times New Roman")
  bot1_name <- text_grob("Tract Node", size = 12, family = "Times New Roman")
  plot_grid <- grid.arrange(
    arrangeGrob(plot_a$group, top = col1_name),
    arrangeGrob(plot_b$group, bottom = bot1_name),
    nrow = 2,
    ncol = 1,
    widths = 1,
    heights = c(1, 1)
  )
  return(plot_grid)
}


#' Draw smooth grid for LGIO models, containing global and difference smooths.
#'
#' @param fit_LGIO mgcv::gam object.
#' @param tract String tract name for title.
#' @param scalar_name String dwi metric for title.
#' @param num_G Number attribute of fit_LGIO holding global smooth.
#' @param num_Ia Number attribute of fit_LGIO holding group A smooth.
#' @param num_Ib Number attribute of fit_LGIO holding group B smooth.
#' @returns Object returned by grid.arrange.
export("grid_lgio")
grid_lgio <- function(
    fit_LGIO, tract, scalar_name, num_G = 2, num_Ia = 3, num_Ib = 4) {
  # Generate plots objs from smooths
  plot_GO <- getViz(fit_LGIO)
  pGlobal <- draw_gs(plot_GO, num_G)
  pDiffa <- draw_gios_diff_sig(plot_GO, num_G, num_Ia)
  pDiffb <- draw_gios_diff_sig(plot_GO, num_G, num_Ib)

  # draw grid
  plot_list <- list(
    "global" = pGlobal,
    "diffa" = pDiffa,
    "diffb" = pDiffb
  )
  name_list <- list(
    "col1" = paste("LGIO:", tract, scalar_name, "Smooths"),
    "rowL1" = "Est. Global Fit",
    "rowL2" = "Diff: Post-Base",
    "rowL3" = "Diff: RTP-Base",
    "bot1" = "Tract Node"
  )
  plot_grid <- .arr_one_three(plot_list, name_list)
  return(plot_grid)
}


#' Draw 2x2 grid of scalar smooths.
#'
#' Assemble output of draw_long_ordered_grid() for FA, MD,
#' AD, and RD into a 2x2 plot.
#'
#' @param plot_FA Plot object of FA smooths.
#' @param plot_MD Plot object of MD smooths.
#' @param plot_AD Plot object of AD smooths.
#' @param plot_RD Plot object of RD smooths.
export("draw_scalar_grid")
draw_scalar_grid <- function(plot_FA, plot_MD, plot_AD, plot_RD) {
  grid.arrange(
    arrangeGrob(plot_FA),
    arrangeGrob(plot_MD),
    arrangeGrob(plot_AD),
    arrangeGrob(plot_RD),
    nrow = 2,
    ncol = 2,
    widths = c(1, 1),
    heights = c(1, 1)
  )
}
