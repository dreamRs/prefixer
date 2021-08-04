
#' Check Rd file
#'
#' @param path Path to Rd files, typically \code{man/}
#'
#' @return Character vector of Rd files path.
#' @export
#' 
#' @name check-rd
#'
#' @examples
#' \dontrun{
#' 
#' # Check if Rd files have examples
#' check_Rd_examples()
#' 
#' # Check if Rd files have value (function return)
#' check_Rd_value()
#' 
#' }
check_Rd_examples <- function(path = "man/") {
  check_Rd_tag(path = path, tag = "\\examples{")
}

#' @export
#' 
#' @rdname check-rd
check_Rd_value <- function(path = "man/") {
  check_Rd_tag(path = path, tag = "\\value{")
}


check_Rd_tag <- function(path = "man/", tag = "\\examples{") {
  tag_ <- gsub(pattern = "[^[:alpha:]]", replacement = "", x = tag)
  Rd <- list.files(path = path, pattern = "Rd$")
  has_tag <- vapply(
    X = Rd,
    FUN = function(file) {
      doc <- readLines(file.path(path, file))
      tag %in% doc
    },
    FUN.VALUE = logical(1)
  )
  if (all(has_tag)) {
    cat(cli::col_green(sprintf("All Rd files have %s.", tag_)), "\n")
  } else {
    cat(cli::col_red(sprintf("%s Rd file(s) with no %s.", sum(!has_tag), tag_)), "\n")
    for (i in which(!has_tag)) {
      cat(cli::col_red(sprintf(" - %s", Rd[i])), "\n")
    }
  }
  invisible(Rd[!has_tag])
}
