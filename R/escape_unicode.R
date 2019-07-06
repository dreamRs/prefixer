
#' Remove all non-ASCII
#'
#' @noRd
#' @importFrom rstudioapi getSourceEditorContext insertText
#' @importFrom stringi stri_escape_unicode stri_locate_all 
#'  stri_sub stri_unescape_unicode stri_detect_regex 
#'
w_escape_unicode <- function() {
  activ_doc <- rstudioapi::getSourceEditorContext()
  script <- activ_doc$contents
  # text between quotes
  loc <- stri_locate_all(str = script, regex = "([\"'])(?:(?=(\\\\?))\\2.)*?\\1")
  # lines starting with #
  comment <- stri_detect_regex(str = script, pattern = "^#")
  # remove lines starting with #
  loc[comment] <- NA
  
  for (i in seq_along(loc)) {
    if (sum(is.na(loc[[i]])) == 0) {
      loc_ <- loc[[i]] + matrix(c(1, -1), ncol = 2, nrow = nrow(loc[[i]]), byrow = TRUE)
      char <- stri_sub(str = script[i], from = loc_)
      char <- stri_escape_unicode(stri_unescape_unicode(char))
      pos <- Map(c, Map(c, i, loc_[, 1]), Map(c, i, loc_[, 2] + 1))
      
      rstudioapi::insertText(location = pos, text = char)
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


#' @importFrom stringi stri_escape_unicode stri_replace_all stri_unescape_unicode
escape_unicode_script <- function(path) {
  script <- readLines(con = path, encoding = "UTF-8")
  script <- paste(script, collapse = "\n")
  script <- stri_escape_unicode(stri_unescape_unicode(script))
  script <- stri_replace_all(str = script, replacement = "'", fixed = "\\'")
  script <- stri_replace_all(str = script, replacement = '"', fixed = '\\"')
  script <- stri_replace_all(str = script, replacement = "\n", fixed = "\\n")
  writeLines(text = script, con = path)
}

