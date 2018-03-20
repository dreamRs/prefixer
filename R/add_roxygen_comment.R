
#' Comment selected lines with \code{#'}
#'
#' @noRd
#' 
#' @importFrom rstudioapi getActiveDocumentContext insertText
#'
add_roxygen_comment <- function() {
  context <- getActiveDocumentContext()
  start <- context$selection[[1]]$range$start["row"]
  end <- context$selection[[1]]$range$end["row"]
  pos <- Map(c, start:end, 1)
  insertText(pos, "#' ")
}





