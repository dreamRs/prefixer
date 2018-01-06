
#' @importFrom stringr str_extract_all str_split
generate_import_from <- function(script) {
  imp <- str_extract_all(string = script, pattern = "[[:alnum:]\\.]+::[[:alnum:]\\._]+")
  if (length(imp[[1]]) > 0) {
    imp <- str_split(imp[[1]], pattern = "::")
    imp <- lapply(X = imp, FUN = matrix, nrow = 1)
    imp <- do.call("rbind", imp)
    imp <- as.data.frame(unique(imp))
    imp <- tapply(X = imp[[2]], INDEX = imp[[1]], FUN = paste, collapse = " ", simplify = FALSE)
    paste("#' @importFrom", names(imp), unlist(imp))
  } else {
    character(0)
  }
}


#' Generate importFrom tag from a function
#'
#' Works only if functions used are prefixed in the body.
#'
#' @param fun A function.
#'
#' @return Character vector
#' @export
#'
#' @examples
#'
#' my_fun <- function(path) {
#'   read.table(file = path, header = FALSE, sep = "\t")
#' }
#' import_from(my_fun)
#'
import_from <- function(fun) {
  body_ <- as.character(body(fun))
  body_ <- paste(body_, collapse = "\n")
  generate_import_from(body_)
}





