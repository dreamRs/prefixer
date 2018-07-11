
#' Comment selected lines with \code{#'}
#'
#' @noRd
#' 
#' @importFrom rstudioapi getSourceEditorContext insertText
#'
v_add_roxygen_comment <- function() {
  context <- rstudioapi::getSourceEditorContext()
  start <- context$selection[[1]]$range$start["row"]
  end <- context$selection[[1]]$range$end["row"]
  pos <- Map(c, start:end, 1)
  rstudioapi::insertText(pos, "#' ")
}





