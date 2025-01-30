
# ---- mpc ----
#' Calculate Post-Marginal Propensity to Consume (MPC) Series
#'
#' This function takes a time series and marginal propensity to consume (MPC)
#' matrix as inputs and calculates the post-MPC series using using matrix 
#' multiplication. [HOW IS THE RESULT INTERPRETED?!?!?]
#'
#' @param x A numeric vector representing the input series.
#' @param mpc_matrix A numeric matrix representing the MPC matrix. The number of rows
#' in the matrix should be equal to the length of the series.
#'
#' @return A numeric vector representing the post-MPC. [MORE DETIAL HOW IS
#' IT INTERPRETED?!?!]
#' @export
#'
#' @examples
#' # THIS example MPC matrix is nonsense. MAke it make sense.
#' series <- c(100, 200, 300, 400)
#' mpc_matrix <- matrix(c(0.5, 0.3, 0.2, 0.1,
#'                        0.4, 0.4, 0.1, 0.1,
#'                        0.3, 0.3, 0.3, 0.1,
#'                        0.2, 0.2, 0.2, 0.4), nrow = 4, byrow = TRUE)
#' calculate_mpc(x = series, mpc_matrix)

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

# ===========================
# Unit-Level Functions (PURCHASES)
# ===========================

# DEFINE MINUS NEUTRAL FUNCTION (PURCHASES)
minus_neutral_purchases <- function(x, # the data in question
                          rpgg, # real potential gdp growth,
                          dg # consumption deflator growth
) {
  output <- (x/lag(x) - dg)^4 - (1+rpgg)
  return(output)
}

# DEFINE SCALE_TO_GDP FUNCTION (PURCHASES)
# Scale to GDP
scale_to_gdp_purchases <- function(x, # the data in question, 
                         gdp, # GDP
                         result)
{
  output = 100*result*(lag(x)/lag(gdp))
  return(output)
}

# ===========================
# Wrapper Function (PURCHASES)
# ===========================

# ---- contribution ----
#' Calculate FIM Contributions, with optional MPCs
#'
#' This function calculates the generic contribution of a time series to GDP
#' growth. It optionally applies an MPC transformation to the input series before
#' calculating the effect on GDP.
contribution_purchases <- function(
    x, 
    rpgg, 
    dg, 
    gdp) {
  
  # Actual Growth Minus Counterfactual Growth
  result <- minus_neutral_purchases(x = x, rpgg = rpgg, dg = dg)
  
  # Apply Scale to GDP Function
  output <- scale_to_gdp_purchases(x = x, result =result, gdp = gdp)
  return(output)
}

# ===========================================
# Unit-Level Functions (TAXES AND TRANSFERS)
# ===========================================

t_counterfactual <- function(x,  # Our data series 
                             c, # Personal Consumption Expenditures
                             rpgg, # Real Potential GDP Growth (quarterly)
                             dg # Deflator Growth (quartelry)
) {
  
  
  counterfactual <- c - x + lag(x)*(1+rpgg+dg)
  return(counterfactual)
}

minus_neutral_t <- function(x,
                            c, 
                            counterfactual
) {
  result <- (c/lag(c))^4 - (counterfactual/lag(c))^4 
  return(result)
}

scale_to_gdp_t <- function(x,
                           gdp,
                           c
) {
  output <- result*(lag(c)/lag(gdp))
  return(output)
}


