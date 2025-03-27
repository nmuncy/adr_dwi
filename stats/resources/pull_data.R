# Various methods for pulling data from MySQL database db_adr.
#
# get_demographics: Get subj_id, sex, and age at baseline from tbl_demo.
# get_user_comp: Get ImPACT user composite measures, and total symptom.
# get_afq: Get AFQ data from requested table.

import("RMySQL")
import(dplyr)
import(lubridate)


#' Establish connection with db_adr.
#'
#' Requires: Local variable 'SQL_PASS' containing
#'  user password for db_adr.
#'
#' @returns Connection object to db_adr.
.db_connect <- function() {
  # host = "127.0.0.1",
  # port = 3308,
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


#' Pull subject age-at-baseline and sex demographics.
#'
#' @returns Tidy dataframe of demographic values.
export("get_demographics")
get_demographics <- function() {
  db_con <- .db_connect()
  sql_cmd <- "select
    td.subj_id, td.sex, td.age_base
    from tbl_demo td
  "
  db_query <- dbSendQuery(db_con, sql_cmd)
  df <- dbFetch(db_query, n = -1)
  dbClearResult(db_query)
  dbDisconnect(db_con)
  return(df)
}


#' Pull ImPACT user composites.
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


#' Build select command for afq rescan data.
#'
#' @returns String of SQL select syntax.
.select_afq_rescan <- function() {
  sql_cmd <- "select
    tafq.subj_id, tafq.sess_id, rft.tract_name, tafq.node_id,
      tafq.dti_fa, tafq.dti_md, tafq.dti_ad, tafq.dti_rd
    from tbl_afq_rescan tafq
    join ref_tract rft on tafq.tract_id=rft.tract_id
  "
  return(sql_cmd)
}


#' Pull AFQ data from db_adr.
#'
#' Get AFQ FA, MD, AD, and RD values for all or
#' specified tracts and sessions. Visit date is
#' also included.
#'
#' @param sess_id (int, 0-2) Optional, identifier of session.
#' @param tract_id (int, 1-28) Optional, identifier of tract.
#' @param table_name (String) Optional, name of table holding pyAFQ metrics.
#' @returns Tidy dataframe of AFQ scalars.
export("get_afq")
get_afq <- function(sess_id = NULL, tract_id = NULL, table_name = "tbl_afq") {
  # Validate user args
  if (!is.null(sess_id)) {
    if (!sess_id %in% 0:2) {
      stop(paste("Unexpected sess_id:", sess_id))
    }
  }
  if (!is.null(tract_id)) {
    if (!tract_id %in% 1:28) {
      stop(paste("Unexpected sess_id:", sess_id))
    }
  }
  if (!table_name %in% c("tbl_afq", "tbl_afq_rerun", "tbl_afq_rescan")) {
    stop(paste("Unexpected table_name:", tbl_name))
  }

  # Build select statement from appropriate table,
  # tbl_afq_rescan is unique and does not have scan dates.
  db_con <- .db_connect()
  if (table_name == "tbl_afq_rescan") {
    sql_cmd <- .select_afq_rescan()
  } else {
    sql_cmd <- "select
      tafq.subj_id, rfs.scan_name, tsd.scan_date,
        rft.tract_name, tafq.node_id,
        tafq.dti_fa, tafq.dti_md, tafq.dti_ad, tafq.dti_rd
    "
    sql_cmd <- paste(sql_cmd, "from", table_name, "tafq") # Query relevant table
    sql_cmd <- paste(
      sql_cmd,
      " join ref_scan rfs on tafq.sess_id=rfs.scan_id
        join ref_tract rft on tafq.tract_id=rft.tract_id
      "
    )
  }

  # If user specifies specific session and/or tract
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


#' Pull and clean impact data.
#'
#' Update impact names and add days post FU1 (diff_post) for
#' and subsequent impact assessments.
#'
#' @param an_dir Location of directory for analyses.
#' @returns Dataframe of impact composite scores.
export("clean_impact")
clean_impact <- function(an_dir) {
  # Check for local csv, read-in or pull from db_adr
  imp_path <- paste(
    an_dir, "dataframes", "df_impact.csv",
    sep = "/"
  )

  if (!file.exists(imp_path)) {
    df <- get_user_comp()
    utils::write.csv(df, imp_path, row.names = F)
    rm(df)
  }
  df_imp <- utils::read.csv(imp_path)

  # Manage column types, names
  df_imp$subj_id <- factor(df_imp$subj_id)
  df_imp$impact_name <- factor(df_imp$impact_name)
  df_imp$impact_date <- as.POSIXct(
    df_imp$impact_date,
    format = "%Y-%m-%d", tz = "UTC"
  )
  colnames(df_imp)[5:10] <- c(
    "mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl", "tot_symp"
  )

  # Calculate days since fu1
  idx_base <- which(df_imp$impact_name == "base")
  df_base <- df_imp[idx_base, ]
  df_post <- df_imp[-c(idx_base), ]
  rm(df_imp)
  df_post <- df_post %>%
    group_by(subj_id, num_tbi) %>%
    mutate(
      date = ymd(impact_date),
      diff_post = as.numeric(date - min(date))
    )
  df_post <- subset(df_post, select = -c(date))
  # names(df_post)[names(df_post) == 'date'] <- 'impact_date'

  # Return combined dfs
  df_base$diff_post <- NA
  df_comb <- rbind(df_base, df_post)
  rm(df_base, df_post)
  return(df_comb)
}
