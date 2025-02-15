#' Get Age Batch List
#'
#' Retrieve header information on all age batches
#'
#' @param process_year Process year to retrieve batches (e.g. 2024 = Apr. 2024 to March 2025)
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
#' getAgeBatchList()
#'
getAgeBatchList <- function(process_year,
                            config_file = "saaWeb.config",
                            user_name = Sys.getenv("username"),
                            password = NULL) {
  config_list <- loadConfigFile(config_file)

  web_conn <-
    openSaaWebConnection(
      config_list$AgeUsageUrl,
      user_name,
      password
    ) |>
    curl::handle_setheaders(Accept = "application/json, text/plain, */*")


  age_batch_list_url <- urlPath(config_list$AgeBatchList, process_year)
  query_response <- curl::curl_fetch_memory(age_batch_list_url, web_conn)

  if (query_response$status_code != HttpStatusOk) {
    stop("Error when retrieving query result: ", query_response$status_code)
  }

  response_content <- query_response$content
  if (is.raw(response_content)) {
    response_content <- rawToChar(response_content)
  }

  response_df <- jsonlite::fromJSON(response_content)

  return(response_df)
}
