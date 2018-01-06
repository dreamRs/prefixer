

#' Remove all prefix in a script
#'
#' @export
#' @importFrom rstudioapi getActiveDocumentContext modifyRange
#'
unprefix <- function() {
  activ_doc <- getActiveDocumentContext()
  script <- activ_doc$contents
  script_ <- str_replace_all(
    string = script, replacement = "",
    pattern = "[[:alnum:]\\.]+::(?=[[:alnum:]\\._]+)"
  )
  insertText(
    location = Map(c, Map(c, seq_along(script), 1), Map(c, seq_along(script), nchar(script) + 1)),
    text = script_
  )
}

