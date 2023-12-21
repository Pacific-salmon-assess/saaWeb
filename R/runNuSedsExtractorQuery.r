#' Run NuSEDs Extractor Query
#'
#' @param query_doc JSON Query file name or text
#' @param config_file Configuration file for SAA Web Services
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
runNusedsExtractorQuery <- function(query_doc,
                                    config_file = "saaWeb.config",
                                    user_name = Sys.getenv("username"),
                                    password = NULL) {
  config_list <- loadConfigFile(config_file)

  extractor_usage_url <- config_list$NusedsExtractorUsageUrl
  extractor_query_url <- config_list$NusedsExtractorQueryUrl

  query_result <- runExtractorQuery(
    query_doc,
    extractor_usage_url,
    extractor_query_url,
    user_name,
    password
  )

  return(query_result)
}
