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
  if(is.null(text)) {
    return(FALSE)
  }

  return(!is.na(text) & nzchar(text))
}
