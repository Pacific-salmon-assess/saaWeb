#' Get Age Batch Details
#'
#' Retrieve the details for the specific age batches
#'
#' @param batch_ids Batch IDs
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
#' AgeBatchDetail(114105)
#'
getAgeBatchDetails <- function(batch_ids,
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

  age_batch_detail_url <- config_list$AgeBatchDetail
  if (is.null(age_batch_detail_url)) {
    stop("No URL provided for the AgeBatchDetail in the config file")
  }

  batch_urls <- urlPath(age_batch_detail_url, batch_ids)

  query_response_list <-
    lapply(
      batch_ids,
      function(., web_conn, age_batch_detail_url) {
        query_url <- urlPath(age_batch_detail_url, .)
        cat("Retrieving batch ", ., " details\n")
        query_response <- curl::curl_fetch_memory(query_url, web_conn)

        if (query_response$status_code != HttpStatusOk) {
          stop("Error when retrieving query result: ", query_response$status_code)
        }
        response_content <- query_response$content
        if (is.raw(response_content)) {
          response_content <- rawToChar(response_content)
        }

        response_df <- jsonlite::fromJSON(response_content)
        return(response_df)
      },
      web_conn,
      age_batch_detail_url
    )
  query_response_df <- do.call(rbind, query_response_list)
  return(query_response_df)
}
