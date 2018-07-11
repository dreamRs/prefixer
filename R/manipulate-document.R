
#' @importFrom rstudioapi setSelectionRanges
set_selection <- function(dat, indice) {
  doc_pos <- get_position(dat, indice)
  rstudioapi::setSelectionRanges(doc_pos)
}

#' @importFrom rstudioapi document_position document_range
get_position <- function(dat, indice) {
  row <- dat$numrow[indice]
  start <- dat$start[indice]
  start_pos <- rstudioapi::document_position(row, start)
  end <- dat$end[indice] + 1
  end_pos <- rstudioapi::document_position(row, end)
  rstudioapi::document_range(start_pos, end_pos)
}

