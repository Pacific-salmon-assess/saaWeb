#' Parse Age Scale Reads
#'
#' @param scale_read_df Data frame of scale age reads
#'
#' @return Parsed and formatted scale age reads
#'
parseAgeScaleReads <- function(scale_read_df) {
  actual_cols <- colnames(scale_read_df)

  expect_cols <- c(
    "Id", "AnalystName", "ProjectName", "ContainerId",
    "Region", "Area", "Species", "GearMrpName", "RecoveryYear",
    "AgingStructure", "ProjectPriority", "LifeHistory",
    "FishNumber", "EuAge", "FishEdge", "GrAge", "ScaleCondition"
  )

  # Do the scale read results have columns we were not expecting?
  unknown_cols <- actual_cols[actual_cols %notin% expect_cols]
  if (length(unknown_cols)) {
    stop(paste0(
      "Unknown columns in scale read results: ",
      paste0(unknown_cols, collapse = ",")
    ))
  }

  missing_cols <- setdiff(expect_cols, actual_cols)
  # Add empty values for missing columns
  scale_read_df[missing_cols] <- NA_character_

  # Reorder the columns to ensure data binding
  scale_read_df <- scale_read_df[expect_cols]

  return(scale_read_df)
}

#' Get Age Batch Scale Results
#'
#' Retrieve the scale age results for the specific batches
#'
#' @param batch_ids Batch IDs to retrieve scale age results
#' @param config_file Configuration file
#' @param user_name User name to execute the query as
#' @param password Password of the user
#'
#' @return Results of the Query
#' @export
#'
#' @importFrom yaml read_yaml
#' @importFrom askpass askpass
#' @importFrom curl new_handle curl_fetch_memory handle_setheaders
#'
#' @examplesIf interactive()
#' getAgeBatchScaleResults(114105)
#'
getAgeBatchScaleResults <- function(batch_ids,
                                    config_file = "saaWeb.config",
                                    user_name = Sys.getenv("username"),
                                    password = NULL) {
  if (allInteger(batch_ids) == FALSE & length(batch_ids) > 0) {
    stop("Age Batch IDs must be integer values.")
  }

  config_list <- loadConfigFile(config_file)

  web_conn <-
    openSaaWebConnection(
      config_list$AgeUsageUrl,
      user_name,
      password
    )

  age_batch_scale_url <- config_list$AgeBatchScaleResults
  if (is.null(age_batch_scale_url)) {
    stop("No URL provided for the AgeBatchScaleResults in the config file")
  }
  query_response_list <-
    lapply(
      batch_ids,
      function(., base_url, web_conn) {
        batch_url <- urlPath(base_url, .)
        query_response <- curl::curl_fetch_memory(batch_url, web_conn)
        cat("Retrieving Scale Ages for batch ", ., "\n")
        if (query_response$status_code != HttpStatusOk) {
          stop("Error when retrieving query result: ", query_response$status_code)
        }
        response_content <- query_response$content
        if (is.raw(response_content)) {
          response_content <- rawToChar(response_content)
        }

        response_df <-
          jsonlite::fromJSON(response_content) |>
          parseAgeScaleReads()

        return(response_df)
      },
      age_batch_scale_url,
      web_conn
    )
  query_response_df <- do.call(rbind, query_response_list)
  return(query_response_df)
}
