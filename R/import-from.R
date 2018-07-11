
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
#' @param quiet Logical, display output to console ?
#'
#' @return Invisible character string
#' @export
#'
#' @examples
#'
#' my_fun <- function(path) {
#'   utils::read.table(file = path, header = FALSE, sep = "\t")
#' }
#' import_from(my_fun)
#'
import_from <- function(fun, quiet = FALSE) {
  body_ <- as.character(body(fun))
  body_ <- paste(body_, collapse = "\n")
  res <- generate_import_from(body_)
  if (!quiet)
    cat(paste(res, collapse = "\n"))
  invisible(paste(res, collapse = "\n"))
}





#' Addin to generate importFrom tag for functions
#'
#' @noRd
#' @importFrom stringr str_which
#' @importFrom rstudioapi getSourceEditorContext
#'
rImportFrom <- function() {
  script <- rstudioapi::getSourceEditorContext()$contents
  script_ <- paste(script, collapse = "\n")
  if.env <- new.env()
  try_parse <- try(eval(parse(text = script_), envir = if.env), silent = TRUE)
  if (class(try_parse) == "try-error") {
    warning("Something went wrong, does your script contains only functions ?")
    return(invisible())
  }
  if_insert <- lapply(
    X = ls(if.env),
    FUN = function(x) {
      if (is.function(if.env[[x]])) {
        tag_if <- import_from(if.env[[x]], quiet = TRUE)
        if (nchar(tag_if) > 0) {
          list(
            importFrom = paste0(tag_if, "\n"),
            num_row = str_which(
              string = script, 
              pattern = paste0("^", x, "[:space:]*(<-|=)[:space:]function")
            )
          )
        } else {
          NULL
        }
      }
    }
  )
  if_insert <- dropNullsOrEmpty(if_insert)
  rstudioapi::insertText(
    location = Map(c, sapply(if_insert, `[[`, "num_row"), 1), 
    text = sapply(if_insert, `[[`, "importFrom")
  )
}




nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0
}
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE=logical(1))]
}

