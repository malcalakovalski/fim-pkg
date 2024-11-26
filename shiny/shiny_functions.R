mutate_where <- function(.data, .where, ...) {
  rows_lgl <- as.logical(rlang::eval_tidy(enquo(.where), .data, parent.frame()))
  .data[rows_lgl,] <- dplyr::mutate(.data[rows_lgl,], ...)
  .data
}

coalesce_join <- function(
    x, y, 
    by = NULL, suffix = c(".x", ".y"), 
    join = dplyr::full_join, ...) {
  
  # Perform the join operation using the specified join function
  joined <- join(x, y, by = by, suffix = suffix, ...)
  
  # Get the union of the column names from both of the joined data frames
  cols <- union(names(x), names(y))
  
  # Identify columns that need to be coalesced (i.e., columns with suffixes)
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  
  # Remove suffixes from the column names and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  # Coalesce columns from the joined data frame (i.e., combine .x and .y columns)
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  # Bind the coalesced columns to the joined data frame and return only the 
  # desired columns
  dplyr::bind_cols(joined, coalesced)[cols]
}