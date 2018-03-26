
#' Remove all non-ASCII
#'
#' @noRd
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' @importFrom stringi stri_escape_unicode stri_locate_all 
#'  stri_sub stri_unescape_unicode
#'
w_escape_unicode <- function() {
  activ_doc <- getActiveDocumentContext()
  script <- activ_doc$contents
  # text between quotes
  loc <- stri_locate_all(str = script, regex = "([\"'])(?:(?=(\\\\?))\\2.)*?\\1")
  for (i in seq_along(loc)) {
    if (sum(is.na(loc[[i]])) == 0) {
      loc_ <- loc[[i]] + matrix(c(1, -1), ncol = 2, nrow = nrow(loc[[i]]), byrow = TRUE)
      char <- stri_sub(str = script[i], from = loc_)
      char <- stri_escape_unicode(stri_unescape_unicode(char))
      pos <- Map(c, Map(c, i, loc_[, 1]), Map(c, i, loc_[, 2] + 1))
      insertText(location = pos, text = char)
    }
  }
  # # comments
  # loc <- stri_locate_all(str = script, regex = "#.*")
  # for (i in seq_along(loc)) {
  #   if (sum(is.na(loc[[i]])) == 0) {
  #     loc_ <- loc[[i]] + matrix(c(1, -1), ncol = 2, nrow = nrow(loc[[i]]), byrow = TRUE)
  #     char <- stri_sub(str = script[i], from = loc_)
  #     char <- stri_escape_unicode(stri_unescape_unicode(char))
  #     pos <- Map(c, Map(c, i, loc_[, 1]), Map(c, i, loc_[, 2] + 1))
  #     insertText(location = pos, text = char)
  #   }
  # }
}

