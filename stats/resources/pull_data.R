import("RMySQL")

#' Establish connection with db_adr.
#'
#' Requires: Local variable 'SQL_PASS' containing
#'  user password for db_adr.
#'
#' @returns Connection object to db_adr.
.db_connect <- function() {
  db_con <- dbConnect(
    MySQL(),
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
#' Deprecated.
#' 
#' @returns Long dataframe of subj_id, scan_name, and scan_date.
export("get_scan_dates")
get_scan_dates <- function() {
  db_con <- .db_connect()
  sql_cmd <- "select
    tsd.subj_id, rfs.scan_name, tsd.scan_date
    from tbl_scan_dates tsd
    join ref_scan rfs on tsd.scan_id=rfs.scan_id
  "
  db_query <- dbSendQuery(db_con, sql_cmd)
  df <- dbFetch(db_query, n = -1)
  dbClearResult(db_query)
  dbDisconnect(db_con)
  return(df)
}

#' Pull Impact user composites.
#'
#' Get composites and total symptom score,
#' also fetch test name and impact dates.
#' 
#' @returns Tidy dataframe of impact composites.
export("get_user_comp")
get_user_comp <- function() {
  db_con <- .db_connect()
  sql_cmd <- "select
    tiu.subj_id, rft.test_name as impact_name,
      tiu.num_tbi, tid.impact_date,
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
  db_query <- dbSendQuery(db_con, sql_cmd)
  df <- dbFetch(db_query, n = -1)
  dbClearResult(db_query)
  dbDisconnect(db_con)

  # Manage random pascal case
  names(df)[names(df) == "UserTotalSymptomScore"] <- "userTotalSymptomScore"
  return(df)
}


#' Pull AFQ data from db_adr.
#' 
#' Get AFQ FA, MD, AD, and RD values for all or
#' specified tracts and sessions. Visit date is
#' also included.
#'
#' @param sess_id (int, 0-2) Optional, identifier of session.
#' @param tract_id (int, 1-28) Optional, identifier of tract.
#' @returns Tidy dataframe of AFQ scalars.
export("get_afq")
get_afq <- function(sess_id = NULL, tract_id = NULL) {
  # Validate user args
  if (!is.null(sess_id)) {
    if (!sess_id %in% 0:2) {
      throw(paste("Unexpected sess_id:", sess_id))
    }
  }
  if (!is.null(tract_id)) {
    if (!tract_id %in% 1:28) {
      throw(paste("Unexpected sess_id:", sess_id))
    }
  }

  # Build select statement, manage user args.
  db_con <- .db_connect()
  sql_cmd <- "select
    tafq.subj_id, rfs.scan_name, tsd.scan_date,
      rft.tract_name, tafq.node_id,
      tafq.dti_fa, tafq.dti_md, tafq.dti_ad, tafq.dti_rd
    from tbl_afq tafq
    join ref_scan rfs on tafq.sess_id=rfs.scan_id
    join ref_tract rft on tafq.tract_id=rft.tract_id
    join tbl_scan_dates tsd on tafq.subj_id=tsd.subj_id
      and tafq.sess_id=tsd.scan_id
  "
  if (!is.null(sess_id) & !is.null(tract_id)) {
    sql_cmd <- paste(
      sql_cmd,
      paste0("where afq.sess_id=", sess_id, " and afq.tract_id=", tract_id)
    )
  } else if (!is.null(sess_id)) {
    sql_cmd <- paste(sql_cmd, paste0("where afq.sess_id=", sess_id))
  } else if (!is.null(tract_id)) {
    sql_cmd <- paste(sql_cmd, paste0("where afq.tract_id=", tract_id))
  }

  # Get and return data.
  db_query <- dbSendQuery(db_con, sql_cmd)
  df <- dbFetch(db_query, n = -1)
  dbClearResult(db_query)
  dbDisconnect(db_con)
  return(df)
}
