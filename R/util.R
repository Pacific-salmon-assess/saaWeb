"%notin%" <- Negate("%in%")


#' Load Configuration File
#'
#' @param config_file_name Configuration file name
#'
#' @importFrom yaml read_yaml
loadConfigFile <- function(config_file_name) {
  if (file.exists(config_file_name) == FALSE) {
    stop("You must have a \"saaWeb.config\" file. As Nicholas Komick about this config file.")
  }

  config_list <- yaml::read_yaml(config_file_name)
  return(config_list)
}


#' Has Non-Zero Text Value
#'
#' Check to see if the parameter has a non-zero text value
#'
#' @param text Text value to check
#'
#' @export
#' @examples
#' hasText(c(NA, "", "Salmon", "   "))
hasText <- function(text) {
  if (is.null(text)) {
    return(FALSE)
  }

  return(!is.na(text) & nzchar(text))
}

#' URL Path
#'
#' Combine parts of a URL path into to a single URL
#'
#' @param ... Segments of the URL to combine
#'
urlPath <- function(...) {
  file.path(..., fsep = "/")
}

#' All Integers
#'
#' Check that all values in a vector are integers
#'
#'
#' @param x Values to check for integers
#' @param ignore_na Ignore NA values
#'
#' @return A single logical value based on all values being integers
#'
#' @examples
#'
#' cetl:::allInteger(letters) # FALSE
#' cetl:::allInteger(c("1", "2", "3", "4", "5", "6", "1000")) # TRUE
#' cetl:::allInteger("3.14") # FALSE
#'
allInteger <- function(x, ignore_na = TRUE) {
  if (is.integer(x)) {
    return(TRUE)
  }

  result <- grepl("^[0-9]+$", x)
  if (ignore_na == TRUE) {
    result <- result | is.na(x)
  }
  return(all(result))
}

#' Get Current Year
#'
#' Get the year value of the current system date
#'
#' @export
#'
#' @examples
#' getCurrentYear()
getCurrentYear <- function() {
  current_year <-
    format(Sys.Date(), "%Y") |>
    as.integer()

  return(current_year)
}
