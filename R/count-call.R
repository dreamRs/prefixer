
#' Count calls in a script or a directory
#'
#' @param path Path to an R script or a directory, in that case all R scripts will be scanned.
#' @param ignore_base Ignore calls from base package.
#' @param find_pkgs Try to find from which package calls are from, need to scan for dependencies with \code{renv}.
#'
#' @return a \code{data.frame} with 2 columns: \code{call} and \code{n}
#' @export
#' 
#' @importFrom renv dependencies
#' @importFrom stats aggregate
#'
count_calls <- function(path, ignore_base = TRUE, find_pkgs = TRUE) {
  path <- normalizePath(path, mustWork = TRUE)
  if (length(path) > 1)
    stop("'path' must be of length 1.", call. = FALSE)
  
  if (dir.exists(path)) {
    paths <- list.files(path = path, pattern = "\\.[rR]$", full.names = TRUE, recursive = TRUE)
    calls <- lapply(paths, count_calls_script)
    calls <- do.call("rbind", calls)
    calls <- aggregate(n ~ call, data = calls, FUN = sum)
  } else {
    calls <- count_calls_script(path)
  }
  if (ignore_base)
    calls <- calls[!in_base(calls$call), ]
  if (isTRUE(find_pkgs)) {
    deps <- renv::dependencies(path = path, quiet = TRUE)
    for (pkg in unique(deps$Package)) {
      requireNamespace(pkg, quietly = TRUE)
    }
    calls$package <- find_pkg(calls$call)
  }
  calls[order(calls$n, decreasing = TRUE), ]
}

count_calls_script <- function(path) {
  parsed <- try(parse(path, keep.source = FALSE), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    data.frame(
      call = character(0), n = numeric(0),
      stringsAsFactors = FALSE
    )
  } else {
    extraded_call <- lapply(parsed, extract_calls)
    extraded_call <- unlist(extraded_call, use.names = FALSE)
    if (length(extraded_call) < 1) {
      return(data.frame(
        call = character(0), n = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
    count_call <- tapply(
      X = extraded_call, 
      INDEX = extraded_call,
      FUN = length,
      simplify = FALSE
    )
    data.frame(
      call = names(count_call), 
      n = unlist(count_call, use.names = FALSE),
      stringsAsFactors = FALSE
    )
  }
}

#' @importFrom utils getAnywhere
find_pkg <- function(funs) {
  if (length(funs) < 1) 
    return(character(0))
  if (length(funs) > 1) {
    unlist(lapply(funs, find_pkg))
  } else {
    where <- getAnywhere(funs)$where
    if (length(where) < 1)
      where <- ""
    if (length(where) > 1)
      where <- paste(where, collapse = ";")
    gsub("namespace:", "", where)
  }
}


in_base <- function(x) {
  unlist(lapply(
    as.character(x), exists,
    where = baseenv()
  ))
}

# By Michael Chirico (https://gist.github.com/MichaelChirico/bab9219308b9507d7183e64f934a9948)
extract_calls = function(exp) {
  if (is.call(exp))
    return(list(as.character(exp[[1L]]), lapply(exp[-1L], extract_calls)))
}

