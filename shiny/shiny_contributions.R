
minus_neutral <- function(x, # the data in question
                          rpgg, # real potential gdp growth,
                          dg # consumption deflator growth
) {
  output <- x - lag(x) * (1 + rpgg) * (1 + dg)
  return(output)
}

mpc <- function(x, mpc_matrix) {
  # Input check that the dimensions of the matrix equal the length of the series
  if (nrow(mpc_matrix) != length(x)) {
    stop("The number of rows in the mpc_matrix must equal the length of the series.")
  }
  
  # Formatting the data as a vertical column matrix is not strictly necessary;
  # but it reinforces the point that this is matrix multiplication
  vert_x <- matrix(x, ncol = 1)
  
  # ensuring proper NA handling by converting to zeroes
  # TODO: Make only the first value of NA equal to 0. Keep the other NAs as NA
  vert_x[is.na(vert_x)] <- 0
  
  # Perform matrix multiplication
  output <- mpc_matrix %*% vert_x
  
  return(output)
}

contribution <- function(x, mpc_matrix = NULL, rpgg, dg, gdp) {
  # If mpc_matrix is not NULL, apply the mpc function first
  if (!is.null(mpc_matrix)) {
    x <- x %>%
      mpc(x = ., mpc_matrix = mpc_matrix)
  }
  
  # Apply the minus_neutral function to x, setting real potential GDP growth
  # and deflator growth inputs to those specified by the arguments.
  result <- x %>%
    minus_neutral(x = ., rpgg = rpgg, dg = dg)
  
  # Apply the scale_to_gdp function
  result %>%
    scale_to_gdp(x = ., gdp = gdp)
}

level <- function(x, mpc_matrix = NULL, rpgg, dg, gdp) {
  # If mpc_matrix is not NULL, apply the mpc function first
  if (!is.null(mpc_matrix)) {
    x <- x %>%
      mpc(x = ., mpc_matrix = mpc_matrix)
  }
  
  # Apply the minus_neutral function to x, setting real potential GDP growth
  # and deflator growth inputs to those specified by the arguments.
  result <- x %>%
    minus_neutral(x = ., rpgg = rpgg, dg = dg)
  
  # Apply the scale_to_gdp function
  result %>%
    scale_to_gdp(x = ., gdp = gdp)
}