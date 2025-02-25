#' Override projections
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
override_projections <- function(df){ 
  override <- readxl::read_excel("data/add-ons/LSFIM_KY_v7.xlsx", 
                         sheet = "FIM Add Factors") %>%
    dplyr::select(date, ends_with('override')) %>%
    mutate(date = lubridate::as_date(date)) 
  
  Q2_2020 <- '2020-06-30'
  Q3_2020 <- '2020-09-30'
  last_override <- '2022-12-31'
  
  df %>%
    left_join(override, by = 'date') %>%
    mutate(unemployment_insurance = if_else(date >= Q3_2020 & date <= last_override,
                                            unemployment_insurance_override,
                                            unemployment_insurance),
           federal_unemployment_insurance = if_else(date >= Q2_2020 & date <= last_override,
                                                    federal_unemployment_insurance_override,
                                                    federal_unemployment_insurance),
           state_unemployment_insurance = if_else(date >= Q2_2020 & date <= last_override,
                                                  state_unemployment_insurance_override,
                                                  state_unemployment_insurance),
           federal_cgrants = if_else(date >= Q2_2020 & date <= Q3_2020,
                                     federal_cgrants_override,
                                     federal_cgrants)
    ) 
}
#' Fill missing values for overrides
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
fill_overrides <- function(df){
  df %>%
    mutate(
      across(
        .cols = contains('unemployment_insurance'),
        .fns = ~if_else(is.na(.), 0, .)
      )
    )
}


create_override <- function(df, var, start, end, values){
  override <- 
    tibble(date = df %>%
             filter(date >= start & date <= end) %>%
             pull(date),
           '{{var}}' := values
    )
  df %>%
    rows_update(override, by = 'date')
}