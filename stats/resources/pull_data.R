library("RMySQL")

#' Establish connection with db_adr.
#' 
#' Requires: Local variable 'SQL_PASS' containing
#'  user password for db_adr.
#'  
#' @returns Connection object to db_adr.
db_connect <- function(){
  db_con <- dbConnect(
    RMySQL::MySQL(),
    dbname = "db_adr",
    host = "localhost",
    port = 3306,
    user = "nmuncy2",
    password = "foobar"
  )
  return(db_con)
}

#' Pull scan dates.
#' 
get_scan_dates <- function(){
  db_con <- db_connect()
  sql_cmd <- "select 
    tsd.subj_id, rfs.scan_name, tsd.scan_date
    from tbl_scan_dates tsd
    join ref_scan rfs on tsd.scan_id=rfs.scan_id
  "
  db_query = dbSendQuery(db_con, sql_cmd)
  df = dbFetch(db_query, n = -1)
  dbClearResult(db_query)
  dbDisconnect(db_con)
  return(df)
}

#' Pull Impact user composites.
#' 
get_user_comp <- function(){
  db_con <- db_connect()
  sql_cmd <- "select 
    tiu.subj_id, rft.test_name as visit_name, 
      tiu.num_tbi, tid.impact_date as visit_date,
      tiu.userMemoryCompositeScoreVerbal,
      tiu.userMemoryCompositeScoreVisual, 
      tiu.userVisualMotorCompositeScore,
      tiu.userReactionTimeCompositeScore, 
      tiu.userImpulseControlCompositeScore,
      tiu.userTotalSymptomScore
    from tbl_impact_user tiu
    join ref_test rft on tiu.test_id=rft.test_id
    join tbl_impact_dates tid on tid.subj_id=tiu.subj_id
      and tid.test_id=tiu.test_id
      and tid.num_tbi=tiu.num_tbi
  "
  db_query = dbSendQuery(db_con, sql_cmd)
  df = dbFetch(db_query, n = -1)
  dbClearResult(db_query)
  dbDisconnect(db_con)
  
  names(df)[names(df) == 'UserTotalSymptomScore'] <- 'userTotalSymptomScore'
  return(df)
}


#' Pull AFQ data from db_adr.
#' 
get_afq <- function(sess_id=NULL, tract_id=NULL){
  # Validate user args
  if(! is.null(sess_id)){
    if(! sess_id %in% 0:2){
      throw(paste("Unexpected sess_id:", sess_id))
    }
  }
  if(! is.null(tract_id)){
    if(! tract_id %in% 1:28){
      throw(paste("Unexpected sess_id:", sess_id))
    }
  }
  
  # Build select statement
  db_con <- db_connect()
  sql_cmd <- "select 
    afq.subj_id, scan.scan_name as sess_name, 
      trct.tract_name, afq.node_id,
      afq.dti_fa, afq.dti_md, afq.dti_ad, afq.dti_rd
    from tbl_afq afq
    join ref_scan scan on afq.sess_id=scan.scan_id
    join ref_tract trct on afq.tract_id=trct.tract_id
  "
  if(! is.null(sess_id) & ! is.null(tract_id)){
    sql_cmd <- paste(
      sql_cmd, 
      paste0("where afq.sess_id=", sess_id, " and afq.tract_id=", tract_id)
    )
  }
  else if(! is.null(sess_id)){
    sql_cmd <- paste(sql_cmd, paste0("where afq.sess_id=", sess_id))
  }
  else if(! is.null(tract_id)){
    sql_cmd <- paste(sql_cmd, paste0("where afq.tract_id=", tract_id))
  }

  #
  db_query = dbSendQuery(db_con, sql_cmd)
  df = dbFetch(db_query, n = -1)
  dbClearResult(db_query)
  dbDisconnect(db_con)
  
  #
  return(df)
}