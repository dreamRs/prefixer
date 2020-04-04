
#' Search for non-ASCII characters in files
#'
#' @param path Path where to start searching. If a directory,
#'  search in all files recursively.
#' @param size_limit Limit size of files scanned (in bytes).
#' @param ignore_dir Ignore some directories (relative to \code{path}).
#' @param ignore_ext Ignore with those extensions.
#'
#' @return a \code{data.frame} with files and lines where
#'  non-ascii have been detected, if none return \code{NULL}.
#' @export
#' 
#' @importFrom stringi stri_enc_isascii
#' @importFrom rstudioapi sourceMarkers
#' @importFrom tools file_ext
#'
#' @examples
#' 
#' \dontrun{
#' 
#' show_nonascii_file(system.file(package = "prefixer"))
#' 
#' }
#' 
show_nonascii_file <- function(path = ".", size_limit = 5e5, 
                               ignore_dir = c("docs"), 
                               ignore_ext = c("png", "jpg", "rds", "rda")) {
  path <- normalizePath(path, mustWork = TRUE)
  files <- list.files(
    path = path, include.dirs = FALSE, 
    full.names = TRUE, no.. = TRUE,
    recursive = TRUE
  )
  extensions <- tolower(tools::file_ext(files))
  files <- files[!extensions %in% tolower(ignore_ext)]
  ignore_dir <- file.path(path, ignore_dir)
  ignore_dir <- Reduce(f = `|`, x = lapply(
    X = ignore_dir, FUN = grepl, 
    x = files, fixed = TRUE
  ))
  files <- files[!ignore_dir]
  files <- grep(
    pattern = "/~$", x = files,
    fixed = TRUE, value = TRUE, invert = TRUE
  )
  files <- grep(
    pattern = "\\.rds$", x = files,
    value = TRUE, invert = TRUE, ignore.case = TRUE
  )
  nonascii <- lapply(
    X = files, 
    FUN = function(x) {
      if (file.size(x) < size_limit) {
        content <- try(readLines(x, warn = FALSE), silent = TRUE)
        if (inherits(content, "try-error")) {
          warning("Unable to inspect", x, call. = FALSE)
        } else {
          nonascii <- stringi::stri_enc_isascii(content)
          if (any(!nonascii)) {
            data.frame(
              file = x,
              line = which(!nonascii),
              column = 1,
              message = content[!nonascii],
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  )
  nonascii <- nonascii[!vapply(nonascii, is.null, FUN.VALUE = logical(1))]
  if (length(nonascii) < 1) {
    rstudioapi::sourceMarkers(
      name = "Non-ASCII",
      markers = data.frame(
        type = "info",
        file = path, 
        line = 1, 
        column = 1, 
        message  = "(non non-ascii characters found)",
        stringsAsFactors = FALSE
      )
    )
    invisible(NULL)
  } else {
    nonascii <- do.call("rbind", nonascii)
    nonascii$type <- "warning"
    rstudioapi::sourceMarkers(
      name = "Non-ASCII",
      markers = nonascii
    )
    invisible(nonascii)
  }
}

