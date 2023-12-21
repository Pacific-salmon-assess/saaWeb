HttpStatusOk <- 200L

#' Parse Extractor Response
#'
#' @param query_response Query response
#'
#' @return Data frame based on the response
#'
#' @importFrom jsonlite fromJSON
#'
parseExtractorResponse <- function(query_response) {
  if (is.raw(query_response)) {
    query_response <- rawToChar(query_response)
  }

  query_json <- jsonlite::fromJSON(query_response, simplifyVector = FALSE)
  data_col_names <- unlist(query_json$Names)

  extractor_data <-
    lapply(
      query_json$Rows,
      function(.) {
        df <- as.data.frame(.$Cells)
        colnames(df) <- data_col_names
        return(df)
      }
    )

  df <- do.call(rbind, extractor_data)
  col_types <- unlist(query_json$Types)

  int_col <- data_col_names[grepl("^int", col_types, ignore.case = TRUE)]
  df[, int_col] <- lapply(df[, int_col], as.integer)

  dbl_col <- data_col_names[grepl("^(single|double|decimal)$", col_types, ignore.case = TRUE)]
  df[, dbl_col] <- lapply(df[, dbl_col], as.double)

  return(df)
}

#' Run Extractor Query
#'
#' @param query_doc JSON Query file name or text
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
#' @examples
#' example_query_file <- system.file("query_doc", "tag_release.qry", package = "saaWeb")
#' runExtractorQuery(example_query_file)
#'
runExtractorQuery <- function(query_doc,
                              extractor_usage_url,
                              extractor_query_url,
                              user_name = Sys.getenv("username"),
                              password = NULL) {
  if (length(query_doc) > 1) {
    stop("The query_doc paramater must be a single value as a file name or json text")
  }

  if (length(query_doc) == 1 && all(file.exists(query_doc)) == TRUE) {
    query_file <- file(query_doc, "rb")
    query_doc <- readBin(query_file, "raw", n = file.info(query_doc)$size)
    close(query_file)
  }

  if (is.character(query_doc)) {
    query_doc_raw <- charToRaw(query_doc)
  } else {
    query_doc_raw <- query_doc
  }

  web_conn <- openSaaWebConnection(
    extractor_usage_url,
    user_name,
    password
  )
  # Further configure the session to POST a query document
  post_query_handle <-
    curl::handle_setopt(web_conn,
      post = TRUE,
      postfieldsize = length(query_doc_raw),
      postfields = query_doc_raw
    ) |>
    curl::handle_setheaders(`Content-Type` = "application/json")

  data_response <- curl::curl_fetch_memory(extractor_query_url, post_query_handle)

  if (data_response$status_code != HttpStatusOk) {
    stop("Error when retrieving query result: ", data_response$status_code)
  }

  query_result <- parseExtractorResponse(data_response$content)

  return(query_result)
}
