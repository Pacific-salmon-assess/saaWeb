#' Get Age Batch List To Current
#'
#' Retrieve header information on all age batches from 2021 to Current Year
#'
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
#' @importFrom dplyr bind_rows
#' @importFrom jsonlite fromJSON
#'
#' @examplesIf interactive()
#' getAgeBatchList2021toCurrent()
#'
getAgeBatchList2021toCurrent <- function(
    config_file = "saaWeb.config",
    user_name = Sys.getenv("username"),
    password = NULL) {

  config_list <- loadConfigFile(config_file)

  web_conn <-
    openSaaWebConnection(
      config_list$AgeUsageUrl,
      user_name,
      password
    )

  age_batch_list_url <- config_list$AgeBatchList
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  years <- seq(2021, current_year)

  query_response_list<-
    lapply(
      years,
      function(years, web_conn, age_batch_list_url){
        query_url <- urlPath(age_batch_list_url, years)
        query_response <- curl::curl_fetch_memory(query_url, web_conn)

        if (query_response$status_code != HttpStatusOk) {
          stop("Error when retrieving query result: ", query_response$status_code)
        }
        response_content <- query_response$content
        if (is.raw(response_content)) {
          response_content <- rawToChar(response_content)
        }

        response_df <- jsonlite::fromJSON(response_content)
        if (is.null(response_df)){
        }else{
          return(response_df)
        }
      },
      web_conn,
      age_batch_list_url
    )

  query_response_df <- bind_rows(query_response_list)
  return(query_response_df)
}
