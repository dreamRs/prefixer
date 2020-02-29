
#' @title Check \code{importFrom} in \code{NAMESPACE}
#' 
#' @description Checks if the functions declared in the NAMESPACE 
#' importFrom are actually used in the package functions.
#' 
#' @param path Path to package directory.
#'
#' @return Functions not used invisibly.
#' @export
#' 
#' @importFrom stringi stri_subset stri_split_regex stri_subset_regex stri_detect_fixed stri_c
#'
#' @examples
#' \dontrun{
#' 
#' # Execute in a package directory
#' check_import_from()
#' 
#' }
check_import_from <- function(path = ".") {
  if (!file.exists(file.path(path, "NAMESPACE")))
    stop("No NAMESPACE to check.", call. = FALSE)
  
  # search funs in importFrom
  namespace <- readLines(con = file.path(path, "NAMESPACE"), warn = FALSE)
  importFrom <- stri_subset(str = namespace, regex = "^importFrom")
  importFrom <- stri_split_regex(str = importFrom, pattern = "\\(|,|\\)")
  importFrom <- lapply(importFrom, `[`, 3)
  importFrom <- unlist(importFrom)
  
  # Read all R scripts
  r_scripts <- list.files(path = file.path(path, "R"), pattern = "(R|r)$")
  r_scripts <- lapply(
    X = file.path(path, "R", r_scripts),
    FUN = readLines, warn = FALSE
  )
  r_scripts <- unlist(r_scripts)
  r_scripts <- stri_subset_regex(str = r_scripts, pattern = "^#", negate = TRUE)
  
  # Check if funs appears
  appears <- lapply(
    X = importFrom,
    FUN = function(x) {
      any(stri_detect_fixed(str = r_scripts, pattern = x))
    }
  )
  appears <- unlist(appears)
  appears <- importFrom[!appears]
  if (length(appears) == 0) {
    message("All functions in @importFrom are used !")
    return(invisible(character(0)))
  } else {
    warning(paste("These functions do not seem to be used:", stri_c(appears, collapse = ", ")), call. = FALSE)
    return(invisible(appears))
  }
}

