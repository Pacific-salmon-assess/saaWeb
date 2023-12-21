#' Open SAA Web Connection
#'
#' @param usage_url URL for network usage acceptance
#' @param user_name User name to execute the query as
#' @param password Password of the user
#'
#' @return Curl Session Handle
#' @export
#'
#' @importFrom askpass askpass
#' @importFrom curl new_handle curl_fetch_memory handle_setheaders
#'
#' @examplesIf interactive()
#' example_query_file <- system.file("query_doc", "tag_release.qry", package = "saaWeb")
#' runCwtExtractorQuery(example_query_file)
#'
openSaaWebConnection <- function(usage_url,
                                 user_name,
                                 password) {
  if (hasText(password) == FALSE) {
    prompt <- paste0(
      "Please enter your password for ",
      user_name,
      ":"
    )
    password <- askpass::askpass(prompt)
  }

  session_handle <-
    curl::new_handle(
      httpauth = 8L, # Use NTLM authentication
      userpwd = paste0(user_name, ":", password)
    ) |>
    curl::handle_setheaders(Accept = "application/json, text/plain, */*")

  # Accept the usage agreement and setup the authentication cookie in the session
  login_result <- curl::curl_fetch_memory(usage_url, handle = session_handle)

  if (login_result$status_code != HttpStatusOk) {
    stop("Error when setting up connection to web server: ", login_result$status_code)
  }

  return(session_handle)
}
