
#' @title Check Rd file
#' 
#' @description Check documentation files to be sure each function have an example and a return value.
#'
#' @param path Path to Rd files, typically \code{man/}
#' @param ignore_doc_type Ignore specific documentation type.
#'  Default is to ignore documentation for datasets and package. Use \code{NULL} to cjeck all files.
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
check_Rd_examples <- function(path = "man/", ignore_doc_type = c("data", "package")) {
  check_Rd_tag(path = path, tag = "\\examples{", ignore_doc_type = ignore_doc_type)
}

#' @export
#' 
#' @rdname check-rd
check_Rd_value <- function(path = "man/", ignore_doc_type = c("data", "package")) {
  check_Rd_tag(path = path, tag = "\\value{", ignore_doc_type = ignore_doc_type)
}


check_Rd_tag <- function(path = "man/", tag = "\\examples{", ignore_doc_type = c("data", "package")) {
  tag_ <- gsub(pattern = "[^[:alpha:]]", replacement = "", x = tag)
  if (!is.null(ignore_doc_type))
    ignore_doc_type <- sprintf("\\docType{%s}", ignore_doc_type)
  Rd <- list.files(path = path, pattern = "Rd$")
  has_tag <- vapply(
    X = Rd,
    FUN = function(file) {
      doc <- readLines(file.path(path, file))
      if (!is.null(ignore_doc_type) && any(ignore_doc_type %in% doc))
        return(TRUE)
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
