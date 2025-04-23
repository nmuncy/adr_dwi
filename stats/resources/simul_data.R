import(simstudy)

.switch_theta_scalar <- function(scalar){
  knots <- c(0.25, 0.5, 0.75)
  
  theta <- switch(
    scalar,
    "FA" = c(0.1, 0.7, 0.3, 0.9, 0.3, 0.7, 0.1),
    "RD" = c(0.8, 0.3, 0.7, 0.1, 0.7, 0.3, 0.8)
 )
  return(list(
    "theta" = theta, 
    "knots" = knots
  ))
}

#' TODO
#' 
export("simul_tract")
simul_tract <- function(visit, scalar, subj_id, adj_range = NULL, adj_amount = NULL){
  
  df_def <- defData(varname = "node", formula = "1;100", dist = "uniform")
  df_node <- genData(1000, df_def)
  
  theta_knots <- .switch_theta_scalar(scalar)
  df_node <- genSpline(
    dt = df_node, newvar = scalar, predictor = "node",
    theta = theta_knots$theta, knots = theta_knots$knots, degree = 3,
    newrange = "40;85", noise.var = 64
  )

  df_node <- as.data.frame(df_node)
  df_node$Visit <- visit
  df_node[, scalar] <- df_node[, scalar] / 100
  df_node$subj <- subj_id
  df_node$Beh <- beh_value <- round(stats::runif(1, min=80, max=100), 0)
  
  if(!is.null(adj_range) & !is.null(adj_amount)){
    idx_scalar <- grep(scalar, colnames(df_node))
    idx_dec <- which(df_node$node > adj_range[1] & df_node$node < adj_range[2])
    df_node[idx_dec, idx_scalar] <- df_node[idx_dec, idx_scalar] + adj_amount
    
    if(scalar == "FA"){
      scale_value <- min(df_node[idx_dec, idx_scalar])
    } else if(scalar == "RD"){
      scale_value <- max(df_node[idx_dec, idx_scalar])
    }
    
    if(visit == "Post"){
      denom <- 2
    } else if(visit == "RTP"){
      denom <- 4
    }
    df_node$Beh <- beh_value - (100 * (1 - scale_value)) / denom
  }
  return(df_node)
}
