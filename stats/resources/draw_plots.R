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

quick_stats <- use("resources/quick_stats.R")

.meas_names <- function(meas) {
  # Switch variable name for something more readable.
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


#' PCA eigenvector
#' TODO
export("draw_pca")
draw_pca <- function(stats_pc) {
  plot_eig <- fviz_eig(stats_pc, addlabels = T)
  plot_bip <- fviz_pca_biplot(stats_pc, label = "var")
  return(list("plot_eig" = plot_eig, "plot_biplot" = plot_bip))
}


#' K-means cluster
#' TODO
export("draw_kmeans")
draw_kmeans <- function(data_norm, clust_km) {
  plot_kmean <- fviz_cluster(list(data = data_norm, cluster = clust_km))
  return(plot_kmean)
}


#' Impact pairs
#' TODO
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


export("visit_box")
visit_box <- function(col_name, df) {
  # Identify subjects who get better from base to fu1
  #
  # Arguments:
  #
  stats_b <- quick_stats$wc_ranksum(col_name, df, "base")
  stats_f <- quick_stats$wc_ranksum(col_name, df, "fu1")

  ggplot(
    df,
    aes(x = visit_name, y = get(col_name), fill = fu1_change),
  ) +
    geom_boxplot() +
    ylab(.meas_names(col_name)) +
    ggtitle(paste0(
      "base: bVw p=",
      round(stats_b$p.value, 3),
      "; fu1: bVw p=",
      round(stats_f$p.value, 3)
    ))
}


export("draw_global_smooth")
draw_global_smooth <- function(plot_obj, attr_num, tract) {
  # Draw global smooth of AFQ tract.
  #
  # Draw global smooth resulting from GS model. Use plot(sm(obj, int))
  # to extract values, calculate confidence intervals, and ggplot
  # on a scaled X-axis.
  #
  # Arguments:
  #   plot_obj (list) = Plotable object returned by getViz
  #   attr_num (int) = List/attribute number of plot_obj that contains
  #     global smooth
  #   tract (str) = AFQ tract name

  # use plot to extract attribute of interest
  p <- plot(sm(plot_obj, attr_num))
  p_data <- as.data.frame(p$data$fit)
  colnames(p_data) <- c("nodeID", "est", "ty", "se")
  p_data$lb <- as.numeric(p_data$est - (2 * p_data$se))
  p_data$ub <- as.numeric(p_data$est + (2 * p_data$se))

  # make, save ggplot
  pp <- ggplot(data = p_data, aes(x = .data$nodeID, y = .data$est)) +
    geom_line() +
    geom_ribbon(aes(ymin = .data$lb, ymax = .data$ub), alpha = 0.2) +
    scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
    theme(
      text = element_text(family = "Times New Roman"),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  # print(pp)
  return(list("global" = pp))
}


export("draw_group_smooth")
draw_group_smooth <- function(plot_obj, attr_num, tract) {
  # Draw group smooths of AFQ tract.
  #
  # Draw group smooths resulting from a GS model. Use plot(sm(obj, int))
  # to extract values and ggplot on a scaled Y- and X-axis.
  #
  # Arguments:
  #   plot_obj (list) = Plotable object returned by getViz
  #   attr_num (int) = List/attribute number of plot_obj that contains
  #     group smooths
  #   tract (str) = AFQ tract name

  # use plot to extract attribute of interest
  p <- plot(sm(plot_obj, attr_num))
  p_data <- as.data.frame(p$data$fit)
  colnames(p_data) <- c("nodeID", "est", "ty", "Group")

  # make, save ggplot
  # limits = c(-0.2, 0.2)
  pp <- ggplot(
    data = p_data,
    aes(x = .data$nodeID, y = .data$est, group = .data$Group)
  ) +
    geom_line(aes(color = .data$Group)) +
    scale_y_continuous() +
    scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
    scale_color_discrete(name = "") +
    theme(
      text = element_text(family = "Times New Roman"),
      legend.position = c(0.88, 0.85),
      legend.text = element_text(size = 8),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  # print(pp)
  return(list("group" = pp))
}


.find_sig_nodes <- function(p_data, sig_dir="less"){
  # find sig nodes
  if(sig_dir == "less"){
    sig_rows <- which(p_data$est < 0 & p_data$ub < 0)
    y_start <- min(p_data$lb)
    y_end <- 0
  }else if(sig_dir == "more"){
    sig_rows <- which(p_data$est > 0 & p_data$lb > 0)
    y_start <- 0
    y_end <- max(p_data$ub)
  }
  sig_nodes <- p_data[sig_rows, ]$nodeID
  
  if(length(sig_nodes) == 0){
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


export("draw_group_smooth_diff")
draw_group_smooth_diff <- function(plot_obj, grp_num, attr_num, tract) {
  # Draw difference of group smooths of AFQ tract.
  #
  # Plot an A-B difference smooth from an GAM using an ordered factor for group,
  # identify nodes which sig differ from 0, draw polygons to ID.
  #
  # Arguments:
  #   plot_obj (list) = Plotable object returned by getViz
  #   attr_num (int) = List/attribute number of plot_obj that contains group
  #     difference smooth
  #   tract (str) = AFQ tract name
  
  # Determine ymin/max from group smooth
  p <- plot(sm(plot_obj, grp_num))
  p_data <- as.data.frame(p$data$fit)
  colnames(p_data) <- c("nodeID", "est", "ty", "se")
  gy_min <- min(p_data$est)
  gy_max <- max(p_data$est)

  # unpack difference smooth data
  p <- plot(sm(plot_obj, attr_num)) +
    geom_hline(yintercept = 0)
  p_data <- as.data.frame(p$data$fit)
  colnames(p_data) <- c("nodeID", "est", "ty", "se")
  p_data$lb <- as.numeric(p_data$est - (1.96 * p_data$se))
  p_data$ub <- as.numeric(p_data$est + (1.96 * p_data$se))
  
  # Determine highest/lowest max/min across global and group smooths
  GY_max <- pmax(gy_max, max(p_data$est))
  GY_min <- pmin(gy_min, min(p_data$est))
  
  # Find nodes that differ
  rect_less <- .find_sig_nodes(p_data, sig_dir="less")
  rect_more <- .find_sig_nodes(p_data, sig_dir="more")
  
  if(is.data.frame(rect_less) & is.data.frame(rect_more)){
    d_rect <- rbind(rect_less, rect_more)
  }else if(is.data.frame(rect_less)){
    d_rect <- rect_less
  }else if(is.data.frame(rect_more)){
    d_rect <- rect_more
  }else{
    pp <- ggplot(data = p_data, aes(x = .data$nodeID, y = .data$est)) +
      geom_hline(yintercept = 0) +
      geom_line() +
      geom_ribbon(
        aes(ymin = .data$lb, ymax = .data$ub),
        alpha = 0.2
      ) +
      scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
      coord_cartesian(ylim=c(GY_min,GY_max)) +
      theme(
        text = element_text(family = "Times New Roman"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
      )
    return(list("diff" = pp))
  }
  

  # # find sig nodes
  # sig_rows <- which(
  #   (p_data$est < 0 & p_data$ub < 0) |
  #     (p_data$est > 0 & p_data$lb > 0)
  # )
  # sig_nodes <- p_data[sig_rows, ]$nodeID
  
  # # Account for lag of significance
  # if(dim(d_rect)[1] == 0){
  #   pp <- ggplot(data = p_data, aes(x = .data$nodeID, y = .data$est)) +
  #     geom_hline(yintercept = 0) +
  #     geom_line() +
  #     geom_ribbon(
  #       aes(ymin = .data$lb, ymax = .data$ub),
  #       alpha = 0.2
  #     ) +
  #     scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  #     coord_cartesian(ylim=c(GY_min,GY_max)) +
  #     theme(
  #       text = element_text(family = "Times New Roman"),
  #       axis.title.y = element_blank(),
  #       axis.title.x = element_blank()
  #     )
  #   return(list("diff" = pp))
  # }

  # # find start, end points of sig regions
  # vec_start <- sig_nodes[1]
  # vec_end <- vector()
  # y_min <- min(p_data$lb)
  # num_nodes <- length(sig_nodes)
  # c <- 2
  # while (c < num_nodes) {
  #   cc <- c + 1
  #   if (sig_nodes[cc] > sig_nodes[c] + 1) {
  #     vec_end <- append(vec_end, sig_nodes[c])
  #     vec_start <- append(vec_start, sig_nodes[cc])
  #   }
  #   c <- cc
  # }
  # vec_end <- append(vec_end, sig_nodes[num_nodes])
  # 
  # # make df for drawing rectangles
  # d_rect <- data.frame(
  #   x_start = vec_start,
  #   x_end = vec_end,
  #   y_start = rep(y_min, length(vec_start)),
  #   y_end = rep(0, length(vec_start))
  # )
  # d_rect$x_start <- d_rect$x_start
  # d_rect$x_end <- d_rect$x_end

  # draw
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
    scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
    coord_cartesian(ylim=c(GY_min,GY_max)) +
    theme(
      text = element_text(family = "Times New Roman"),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  # print(pp)
  return(list("diff" = pp))
}


export("draw_one_three")
draw_one_three <- function(plot_list, name_list, tract) {
  # Assemble a 1x3 grid of plots.
  #
  # For plotting tract global, group, difference smooths.
  #
  # Arguments:
  #   plot_list (list) = contains $global, $group, and $diff
  #   name_list (list) = contains column, left, right, and bottom names
  #   tract (str) = AFQ tract name

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
  # print(pOut)

  # ggsave(
  #   paste0(out_dir, "/Plot_", tract, "_smooths.png"),
  #   plot = pOut,
  #   units = "in",
  #   height = 6,
  #   width = 3,
  #   dpi = 600,
  #   device = "png"
  # )
  return(pOut)
}



export("draw_one_four")
draw_one_four <- function(plot_list, name_list, tract) {
  # unpack, organize plots
  r1A <- plot_list$global$global
  r2A <- plot_list$group$group
  r3A <- plot_list$diffa$diff
  r4A <- plot_list$diffb$diff

  # make col titles, y axis, x axis, and row names
  col1_name <- text_grob(name_list$col1, size = 12, family = "Times New Roman")
  bot1_name <- text_grob(name_list$bot1, size = 10, family = "Times New Roman")

  l1_name <- l2_name <-
    text_grob(name_list$rowL1, size = 12, family = "Times New Roman", rot = 90)
  l3_name <- l4_name <-
    text_grob(name_list$rowL2, size = 12, family = "Times New Roman", rot = 90)

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
  # print(pOut)

  # ggsave(
  #   paste0(out_dir, "/Plot_", tract, "_smooths.png"),
  #   plot = pOut,
  #   units = "in",
  #   height = 6,
  #   width = 3,
  #   dpi = 600,
  #   device = "png"
  # )
  return(pOut)
}

export("draw_two_three")
draw_two_three <- function(plot_list, name_list, tract, beh_short, out_name) {
  # Assemble a 2x3 grid of plots.
  #
  # For plotting covariate and interaction smooths for control, experimental,
  # and difference.
  #
  # Arguments:
  #   plot_list (list) = contains $global, $group, and $diff
  #   name_list (list) = contains column, left, right, and bottom names
  #   tract (str) = AFQ tract name
  #   beh_short (str) =  identifier for specific covariate
  #   out_name (str) = type of analysis (LGI/ROI/PPI)

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

  # ggsave(
  #   paste0(out_dir, "/Plot_", tract, "_", out_name, "_", beh_short, ".png"),
  #   plot = pOut,
  #   units = "in",
  #   height = 6,
  #   width = 6,
  #   dpi = 600,
  #   device = "png"
  # )
}



#' Draw smooth grid.
#'
#' TODO
export("draw_long_ordered_grid")
draw_long_ordered_grid <- function(
    fit_LGSIO, tract, scalar_name, num_G = 2, num_GOa = 3, num_GOb = 4) {
  
  # Generate plots objs from smooths
  plot_GO <- getViz(fit_LGSIO)
  pGlobal <- draw_global_smooth(plot_GO, num_G, tract)
  pDiffa <- draw_group_smooth_diff(plot_GO, num_G, num_GOa, tract)
  pDiffb <- draw_group_smooth_diff(plot_GO, num_G, num_GOb, tract)

  # draw grid
  plot_list <- list(
    "global" = pGlobal,
    "diffa" = pDiffa,
    "diffb" = pDiffb
  )
  name_list <- list(
    "col1" = paste(tract, scalar_name, "Smooths"),
    "rowL1" = "Est. Global Fit",
    "rowL2" = "Diff: Post-Base",
    "rowL3" = "Diff: RTP-Base",
    "bot1" = "Tract Node"
  )
  plot_grid <- draw_one_three(plot_list, name_list, tract)
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


#' Draw smooth grid.
#'
#' TODO
export("draw_long_grid")
draw_long_grid <- function(
    fit_LGSI, fit_LGSIO, tract, scalar_name,
    num_G = 2, num_GS = 3, num_GOa = 3, num_GOb = 4) {
  # Generate plots objs from smooths
  plot_G <- getViz(fit_LGSI)
  pGlobal <- draw_global_smooth(plot_G, num_G, tract)
  pGroup <- draw_group_smooth(plot_G, num_GS, tract)

  plot_GO <- getViz(fit_LGSIO)
  pDiffa <- draw_group_smooth_diff(plot_GO, num_GOa, tract)
  pDiffb <- draw_group_smooth_diff(plot_GO, num_GOb, tract)

  # draw grid
  plot_list <- list(
    "global" = pGlobal,
    "group" = pGroup,
    "diffa" = pDiffa,
    "diffb" = pDiffb
  )
  name_list <- list(
    "col1" = paste(tract, "Smooths"),
    "rowL1" = paste("Est.", scalar_name, "Fit"),
    "rowL2" = paste("Est.", scalar_name, "Diff"),
    "rowR1" = "Global",
    "rowR2" = "Group",
    "rowR3" = "Post-Base",
    "rowR4" = "RTP-Base",
    "bot1" = "Tract Node"
  )
  plot_grid <- draw_one_four(plot_list, name_list, tract)
  return(plot_grid)
}
