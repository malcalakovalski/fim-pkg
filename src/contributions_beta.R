# src/contributions.R
# This module defines 4 functions, used to transform a raw data series
# into a FIM contribution. Read more to find out what each function does.
#
# UNIT FUNCTIONS
# The unit functions define specific steps that are applied to applied to input 
# data to eventually produce FIM output ("contributions"). These steps are:
# 1. minus_neutral, 
# 2. mpc, 
# 3. scale_to_gdp.
# For more information on what each step's purpose is, read the Roxygen documentation
# below.
#
# WRAPPER FUNCTIONS (there's only one)
# 4. contribution
# contribution is a wrapper function that bundles the first 3 functions together
# to produce FIM output ("contributions"). As an input, it takes the raw data 
# vector and necessary accessory variables. Its output is the quarterly FIM contribution
# from that variable. 
# 
# Not all input data needs to go through all 3 unit functions in order to produce
# FIM output. Some input data needs to only go through the minus_neutral and the
# scale_to_gdp step. The mpc step can be optionally skipped by assigning the mpc_matrix
# argument to NULL.
# 
# Why no MPC? Federal purchases, state purchases, investment grants, and consumption 
# grants all fall in this category, since they measure direct effects of government 
# spending on output. (In other words, their effects on GDP are instantaneous). 
# Our supply side IRA variable also falls in this category since it already represents
# a direct estimate of how the CHIPS/IRA legislation increased manufacturing output.
#
# Why use an MPC? We apply MPCs when we do believe that the effect of a cash flow 
# on GDP is not immediate. For example, unemployment benefits might be received 
# in quarter 1, but not spent until quarter 2. And some of the initial disbursement
# might be saved permanently. Thus, MPCs capture timing effects of taxes/transfers
# on GDP as well as the fact that these taxes and transfers may not have a 1:1 or
# immediate effect on output.
#
# ===========================
# Unit-Level Functions
# ===========================

# ---- post_minus_neutral ----
#' Calculate Post-Minus Neutral Series
#' 
#' This function takes time series data, real potential GDP growth, and
#' consumption deflator growth as input. It calculates a counterfactual 
#' situation where each subsequent point in the input time series is the 
#' product of the previous observation, the consumption deflator, and 
#' potential GDP growth. It then calculates how quickly in excess of or below
#' real potential GDP growth the input data series is growing. The output 
#' can be interpreted as [WHAT? POPULATE THIS? WHAT UNITS? BUILLIONS USD? REAL
#' OR NOT?]
#' 
#' @param x A numeric vector representing the input series in billions USD.
#' @param rpgg A numeric vector representing the real potential GDP growth, as an
#' annualized proportion. For example, 3% annualized real potential GDP growth would
#' be represented as 0.03. [A -1$ anualized rpgg would bre represented as YY]
#' @param dg A numeric vector representing the deflator growth, as an annualized 
#' proportion. For example, 3% annualized deflator growth would be represented 
#' as 0.03. [give negative example too]
#'
#' @return A numeric vector representing the "minus neutral" series
#' # TODO: improve this description of output. too vague
#' @export
minus_neutral <- function(x, # the data in question
                          rpgg, # real potential gdp growth,
                          dg # consumption deflator growth
) {
  
  output <- ( (x / lag(x) )^4 * lag(x) - lag(x)* (1 + dg + rpgg) ) * 100

  # Convert NaNs at the end of the vector to zero
  # This is because dividing by 0s in the line above leads to NaNs
  nan_index <- which(is.nan(output))  # Identify NaNs
  if (length(nan_index) > 0) {
    output[nan_index] <- 0  # Replace NaNs with zero
  }

  # This is the correct, calculation, but it affects minus_neutral
  #output <- x - (lag(x) * (1 + rpgg) * (1 + dg))
  # This optional line will make the 1970 Q1 entries equal a numeric value, rather
  # than NA, by assuming that the 1969 Q4 value for each data series was 0. 
  # Prior versions of the FIM had this setting on for pandemic-era stimulus. We
  # have chosen to remove it.
  # output <- x - lag(x, default = 0) * (1 + rpgg + dg)
  return(output)
}

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

# ---- scale_to_gdp ----
#' Scale to GDP
#'
#' This function calculates the the generic contribution of a time series to GDP
#' growth.
#'
#' @param x A numeric vector representing the input series in billions USD.
#' @param gdp A numeric vector representing the GDP, in billions USD.
#'
#' @return A numeric vector representing the contribution of the input series to
#' GDP growth.
#' @export
#'
#' @examples
#' # Example usage:
#' generic_contribution(
#'   x = c(3219.7, 3249.7, 3280.6, 3311.4, 3343.7),
#'   gdp = c(29314, 29626, 29942, 30268, 30577)
#' )
scale_to_gdp <- function(x, gdp) {
  output = x / lag(gdp)
  return(output)
}


# ===========================
# Wrapper Functions
# ===========================

# ---- contribution ----
#' Calculate FIM Contributions, with optional MPCs
#'
#' This function calculates the generic contribution of a time series to GDP
#' growth. It optionally applies an MPC transformation to the input series before
#' calculating the effect on GDP.
#'
#' @param x A numeric vector representing the input series in billions USD.
#' @param gdp A numeric vector representing the GDP, in billions USD.
#' @param rpgg TODO
#' @param dg A numeric vector representing the deflator growth, as an annualized 
#' proportion. For example, 3% annualized deflator growth would be represented 
#' as 0.03. [give negative example too]
#' @param mpc_matrix A matrix representing the MPCs. If set to NULL, the MPC step 
#' is skipped.
#'
#' @return A numeric vector representing the contribution of the input series to
#' GDP growth.
#' @export
#'
#' @examples
#' # Example usage:
#' #TODO
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

# ---- levels ----
#' Calculate FIM Levels
#'
#' This function calculates the generic contribution of a time series to GDP
#' cumulative growth. It optionally applies an MPC transformation to the input 
#' series before calculating the effect on GDP.
#'
#' @param x A numeric vector representing the input series in billions USD.
#' @param gdp A numeric vector representing the GDP, in billions USD.
#' @param rpgg TODO
#' @param dg A numeric vector representing the deflator growth, as an annualized 
#' proportion. For example, 3% annualized deflator growth would be represented 
#' as 0.03. [give negative example too]
#' @param mpc_matrix A matrix representing the MPCs. If set to NULL, the MPC step 
#' is skipped.
#'
#' @return A numeric vector representing the contribution of the input series to
#' GDP growth.
#' @export
#'
#' @examples
#' # Example usage:
#' #TODO
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