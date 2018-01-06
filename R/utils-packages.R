

#' List all functions in search path and their package
#'
#' @return a \code{data.frame}
#' @noRd
#'
#' @examples
#'
#' get_package_funs()
#'
get_package_funs <- function() {
  search_path <- .packages()
  result <- lapply(
    X = search_path, FUN = function(x) {
      funs <- ls(paste0("package:", x))
      if (length(funs) > 0) {
        data.frame(
          package = x,
          funs = funs,
          stringsAsFactors = FALSE
        )
      }
    }
  )
  do.call("rbind", result)
}
# get_package_funs <- function(all.available = FALSE) {
#   search_path <- .packages(all.available = all.available)
#   result <- lapply(
#     X = search_path, FUN = function(x) {
#       funs <- getNamespaceExports(ns = x)
#       if (length(funs) > 0) {
#         data.frame(
#           package = x,
#           funs = funs,
#           stringsAsFactors = FALSE
#         )
#       }
#     }
#   )
#   do.call("rbind", result)
# }







#' Get packages load with "library" or "require" in script and not in search path
#'
#' @param script A script (character string)
#'
#' @return A character vector
#' @noRd
#'
#' @importFrom stringr str_extract_all str_replace_all str_trim
#'
#' @examples
#' get_unloaded_packages("library(dplyr); iris %>% arrange(Sepal.Length)")
get_unloaded_packages <- function(script) {
  packs <- str_extract_all(string = script, pattern = "(?<=(library|require)\\()[[:alnum:]\\.'\"]+(?=\\))")
  packs <- unlist(packs)
  packs <- str_replace_all(string = packs, pattern = "\"|'", replacement = "")
  packs <- str_trim(string = packs)
  packs[!packs %in% .packages()]
}




