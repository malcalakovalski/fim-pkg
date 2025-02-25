#' Mutate on rows using logical subsetting 
#'
#' @param .data 
#' @param .where 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
mutate_where <- function(.data, .where, ...) {
  rows_lgl <- as.logical(rlang::eval_tidy(enquo(.where), .data, parent.frame()))
  .data[rows_lgl,] <- dplyr::mutate(.data[rows_lgl,], ...)
  .data
}
