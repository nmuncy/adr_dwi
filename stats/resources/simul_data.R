# Simulate data with expected properties to visualize hypotheses.

import(simstudy)

#' Return theta and knots for FA, RD.
.switch_theta_scalar <- function(scalar) {
  knots <- c(0.25, 0.5, 0.75)
  theta <- switch(scalar,
    "FA" = c(0.1, 0.7, 0.3, 0.9, 0.3, 0.7, 0.1),
    "RD" = c(0.5, 0.3, 0.7, 0.1, 0.7, 0.3, 0.5)
  )
  return(list(
    "theta" = theta,
    "knots" = knots
  ))
}


#' Return lower, upper bound of behaviors by visit.
.switch_visit_beh <- function(visit) {
  start_end <- switch(visit,
    "Base" = c(90, 100),
    "Post" = c(65, 90),
    "RTP" = c(85, 95)
  )
  return(start_end)
}


#' Adjust tract and behavior values by visit.
.adjust_tract_beh <- function(
    df_node, scalar, visit, adj_range, adj_amount, beh_value) {
  # Change scalars within range by amount
  idx_scalar <- grep(scalar, colnames(df_node))
  idx_dec <- which(df_node$node > adj_range[1] & df_node$node < adj_range[2])
  df_node[idx_dec, idx_scalar] <- df_node[idx_dec, idx_scalar] + adj_amount

  # Identify min FA or max RD
  if (scalar == "FA") {
    scale_value <- min(df_node[idx_dec, idx_scalar])
  } else if (scalar == "RD") {
    scale_value <- max(df_node[idx_dec, idx_scalar])
  }

  # Adjust behavior so post is worse
  if (visit == "Post") {
    denom <- 2
  } else if (visit == "RTP") {
    denom <- 4
  }
  df_node$Beh <- beh_value - (100 * (1 - scale_value)) / denom
  return(df_node)
}


#' Simulate tract and behavior data.
#'
#' Behavior values are shifted by minimum injury node value when adj_range
#' and adj_amount arguments are used.
#'
#' @param visit Name of visit (Base, Post, RTP).
#' @param scalar Name of scalar (FA, RD).
#' @param subj_id Subject ID.
#' @param adj_range Optional, list, range of nodes to apply adjustment.
#' @param adj_amount Optional, numeric, adjust to be added to adjusted nodes.
#' @returns Long-formatted dataframe.
export("simul_tract_beh")
simul_tract_beh <- function(
    visit, scalar, subj_id, adj_range = NULL, adj_amount = NULL) {
  # Setup spline
  df_def <- defData(varname = "node", formula = "1;100", dist = "uniform")
  df_node <- genData(100, df_def)
  theta_knots <- .switch_theta_scalar(scalar)

  # Generate data
  df_node <- genSpline(
    dt = df_node, newvar = scalar, predictor = "node",
    theta = theta_knots$theta, knots = theta_knots$knots, degree = 3,
    newrange = "40;85", noise.var = 0.1
  )

  # Add other dataframe values
  df_node <- as.data.frame(df_node)
  df_node$Visit <- visit
  df_node[, scalar] <- df_node[, scalar] / 100
  df_node$subj <- subj_id

  df_node$Beh <- beh_value <- round(stats::runif(1, min = 80, max = 100), 0)
  # start_end <- .switch_visit_beh(visit)
  # df_node$Beh <- beh_value <- round(stats::runif(1, min=start_end[1], max=start_end[2]), 0)

  # Clean up scalar values and nodes
  df_node[, scalar] <- df_node[sort.int(df_node$node, index.return = T)$ix, scalar]
  df_node$node <- 1:100

  # Adjust scalars and behavior values
  if (!is.null(adj_range) & !is.null(adj_amount)) {
    df_node <- .adjust_tract_beh(
      df_node, scalar, visit, adj_range, adj_amount, beh_value
    )
  }
  return(df_node)
}
