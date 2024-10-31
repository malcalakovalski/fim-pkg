#' Data Cleaning Functions for FIM
#'
#' This module contains functions that combine data from various FIM sources to 
#' construct the input series that are used in the model itself. There are 33 (?)
#' functions, each producing a data series, which is a tibble containing a date
#' column and a data_series column.


# ---- Main-variables ----

#' Create Federal Purchases Data Series
#'
#' This function combines federal purchases data from national accounts and forecast sources 
#' into a single data series, filling any gaps with placeholder zeroes extending to 2034 Q3.
#'
#' @param national_accounts A tibble containing national accounts data with a `date` column and a `gf` column for federal purchases.
#' @param forecast A tibble containing forecast data with a `date` column and a `federal_purchases` column.
#' @param placeholder_nas A tibble of appropriate length populated by NAs, extending the date range to 2034 Q3.
#'
#' @return A tibble containing the combined federal purchases data series, with NAs and missing entries replaced by 0s.
#' @export
#'
#' @examples
#' # Assuming `national_accounts`, `forecast`, and `placeholder_nas` are pre-defined tibbles:
#' federal_purchases <- create_federal_purchases(national_accounts, forecast, placeholder_nas)
create_federal_purchases <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_purchases) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_purchases)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # `gf` is the Haver code for federal purchases
    select(date, gf) %>%  
    # Rename data to generic `data_series` for easier merging
    rename(data_series = gf) 
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}



#' Create Consumption Grants Data Series
#'
#' This function combines consumption grants data from national accounts, forecast sources, 
#' and historical overrides into a single data series, filling any gaps with placeholder zeroes extending to 2034 Q3.
#'
#' @param national_accounts A tibble containing national accounts data with a `date` column and `gfeg` and `gfeghdx` columns for gross consumption grants and Medicaid grants respectively.
#' @param forecast A tibble containing forecast data with a `date` column and a `consumption_grants` column.
#' @param historical_overrides A tibble containing historical override data with a `date` column and a `consumption_grants_override` column.
#' @param placeholder_nas A tibble of appropriate length populated by NAs, extending the date range to 2034 Q3.
#'
#' @return A tibble containing the combined consumption grants data series, with NAs and missing entries replaced by 0s.
#' @export
#'
#' @examples
#' # Assuming `national_accounts`, `forecast`, `historical_overrides`, and `placeholder_nas` are pre-defined tibbles:
#' consumption_grants <- create_consumption_grants(national_accounts, forecast, historical_overrides, placeholder_nas)
create_consumption_grants <- function(
    national_accounts, 
    forecast, 
    historical_overrides, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, consumption_grants) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = consumption_grants)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Subtract medicaid grants (gfeghdx) from gross consumption grants (gfeg)
    # and assign to the name "data_series"
    mutate(data_series = gfeg - gfeghdx) %>%
    # Keep the date and the new data_series column
    select(date, data_series) %>%
    # Override historic entries of national_accounts using historical_overrides data
    mutate_where(
      date >= yearquarter('2020 Q3') & date <= current_quarter,
      data_series = historical_overrides$consumption_grants_override
    )
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


#' Create Investment Grants Data Series
#'
#' This function combines investment grants data from national accounts, forecast sources, 
#' and historical overrides into a single data series, filling any gaps with placeholder zeroes extending to 2034 Q3.
#'
#' @param national_accounts A tibble containing national accounts data with a `date` column and a `gfeigx` column for investment grants.
#' @param forecast A tibble containing forecast data with a `date` column and an `investment_grants` column.
#' @param historical_overrides A tibble containing historical override data with a `date` column and an `investment_grants_override` column.
#' @param placeholder_nas A tibble of appropriate length populated by NAs, extending the date range to 2034 Q3.
#'
#' @return A tibble containing the combined investment grants data series, with NAs and missing entries replaced by 0s.
#' @export
#'
#' @examples
#' # Assuming `national_accounts`, `forecast`, `historical_overrides`, and `placeholder_nas` are pre-defined tibbles:
#' investment_grants <- create_investment_grants(national_accounts, forecast, historical_overrides, placeholder_nas)
create_investment_grants <- function(
    national_accounts, 
    forecast, 
    historical_overrides, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, investment_grants) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = investment_grants)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # `gfeigx` is the Haver code for investment grants
    select(date, gfeigx) %>%  
    # Rename data to generic `data_series` for easier merging
    rename(data_series = gfeigx) %>%
    # Override historic entries of national_accounts using historical_overrides data
    mutate_where(
      date >= yearquarter('2020 Q3') & date <= current_quarter,
      data_series = historical_overrides$investment_grants_override
    )
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}



#' Create State Purchases Data Series
#'
#' This function combines state purchases data from national accounts and forecast sources 
#' into a single data series, filling any gaps with placeholder zeroes extending to 2034 Q3.
#'
#' @param national_accounts A tibble containing national accounts data with a `date` column and a `gs` column for state purchases.
#' @param forecast A tibble containing forecast data with a `date` column and a `state_purchases` column.
#' @param placeholder_nas A tibble of appropriate length populated by NAs, extending the date range to 2034 Q3.
#'
#' @return A tibble containing the combined state purchases data series, with NAs and missing entries replaced by 0s.
#' @export
#'
#' @examples
#' # Assuming `national_accounts`, `forecast`, and `placeholder_nas` are pre-defined tibbles:
#' state_purchases <- create_state_purchases(national_accounts, forecast, placeholder_nas)
create_state_purchases <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, state_purchases) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = state_purchases)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # `gf` is the Haver code for state purchases
    select(date, gs) %>%  
    # Rename data to generic `data_series` for easier merging
    rename(data_series = gs)
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}



create_federal_non_corporate_taxes <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_non_corporate_taxes) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_non_corporate_taxes)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Calculate federal non corporate taxes as a sum of 3 series
    mutate(data_series = 
             gfrpt + # Haver code for federal personal taxes
             gfrpri + # Haver code for federal production taxes
             gfrs # Haver code for federal payroll taxes
           ) %>%
    # Keep the date and the new data_series column
    select(date, data_series)
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_state_non_corporate_taxes <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, state_non_corporate_taxes) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = state_non_corporate_taxes)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Calculate federal non corporate taxes as a sum of 3 series
    mutate(data_series = 
             gsrpt + # Haver code for state personal taxes
             gsrpri + # Haver code for state production taxes
             gsrs # Haver code for state payroll taxes
    ) %>%
    # Keep the date and the new data_series column
    select(date, data_series)
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_federal_corporate_taxes <- function(
    national_accounts, 
    forecast, 
    historical_overrides, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_corporate_taxes) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_corporate_taxes)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # `gf` is the Haver code for federal purchases
    select(date, gfrcp) %>%  
    # Rename data to generic `data_series` for easier merging
    rename(data_series = gfrcp) %>%
    # Corporate taxes come one quarter later than GDP. We overwrite JUST the current
    # quarter using the historical overrides sheet
    mutate_where(
      date == current_quarter,
      data_series = tail(historical_overrides$federal_corporate_taxes_override, n = 1)
      )

  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_supply_side_ira <- function(
    forecast, 
    historical_overrides, 
    placeholder_nas
) {
  # The only input into supply_side_ira is our historical overrides and our 
  # forecast. This data does not exist anywhere in the national accounts.
  
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, supply_side_ira) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = supply_side_ira)
  
  # Select column of interest from the historical overrides
  historical_overrides <- historical_overrides %>%
    select(date, supply_side_ira_override) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = supply_side_ira_override)
  
  # Merge the historical overrides with the forecast using the commonly named 
  # `data_series` and `date` columns. The historical overrides take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(historical_overrides, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(
      create_placeholder_nas(start = "1970-01-01"), 
      by = 'date'
      ) %>%
    # Make sure the rows are in chronological order
    arrange(date) %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}

create_state_corporate_taxes <- function(
    national_accounts, 
    forecast, 
    historical_overrides, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, state_corporate_taxes) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = state_corporate_taxes)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # `gf` is the Haver code for federal purchases
    select(date, gsrcp) %>%  
    # Rename data to generic `data_series` for easier merging
    rename(data_series = gsrcp) %>%
    # Corporate taxes come one quarter later than GDP. We overwrite JUST the current
    # quarter using the historical overrides sheet
    mutate_where(
      date == current_quarter,
      data_series = tail(historical_overrides$state_corporate_taxes_override, n = 1)
    )
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_federal_social_benefits <- function(
    national_accounts, 
    forecast, 
    historical_overrides,
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_social_benefits) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_social_benefits)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Calculate federal non corporate taxes as a sum of 3 series
    mutate(data_series = 
             # The coalesce function is replacing 0s for NAs before subtracting
             coalesce(gftfp, 0) - # Haver code for federal social benefits
             coalesce(yptu, 0) - # Haver code for ui
             coalesce(gftfpe, 0) - # Haver code for rebate checks
             coalesce(yptmr, 0) - # Haver code for Medicare
             coalesce(gftfpv, 0) # Haver code for nonprofit provider relief fund
    ) %>%
    # Keep the date and the new data_series column
    select(date, data_series) %>%
    # Override historic entries of national_accounts using historical_overrides data
    mutate_where(
      date >= yearquarter('2020 Q3') & date <= current_quarter,
      data_series = historical_overrides$federal_social_benefits_override
    ) %>%
    # Further overwrite by adding 203 to 2021 Q1
    mutate_where(
      date == yearquarter('2021 Q1'),
      data_series = data_series + 203
    )
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_state_social_benefits <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, state_social_benefits) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = state_social_benefits)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Calculate federal non corporate taxes as a sum of 3 series
    mutate(data_series = 
             gstfp - # Haver code for state social benefits
             yptmd # Haver code for Medicaid
    ) %>%
    # Keep the date and the new data_series column
    select(date, data_series)
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_rebate_checks <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, rebate_checks) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = rebate_checks)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # The coalesce function turns NAs into 0s and the gftfpe is the Haver code 
    # for rebate checks
    mutate(data_series = coalesce(gftfpe, 0)) %>%
    # Keep the date and the new data_series column
    select(date, data_series) %>%
    # Note from Lorae: We split up rebate checks into rebate_checks and rebate_checks_arp,
    # and this subtracts away some amount from a quarter where we allocated it to
    # rebate_checks_arp. We do that so that we can apply a different MPC. But TBH this
    # process is pretty illogical. Please don't blame me ¯\_(ツ)_/¯
    # TODO: Stop this nonsense of hard coding values and put them in the historical_overrrides
    # tab of the forecast spreadsheet instead
    mutate_where(
      date == yearquarter('2021 Q1'),
      data_series = data_series - 1348.1
    ) %>%
    mutate_where(
      date == yearquarter("2021 Q4"),
      data_series = 0)
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_rebate_checks_arp <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, rebate_checks_arp) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = rebate_checks_arp)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Hard coding one silly value here ¯\_(ツ)_/¯
    mutate(
      data_series = if_else(
        date == yearquarter("2021 Q1"),
        1348.1, # Rebate checks arp are 1348.1 in 2021 Q1
        0) # Otherwise they are 0
      )  %>%
    # Hard coding another silly value
    mutate_where(
      date == yearquarter("2021 Q4"),
      data_series = 14.2
      ) %>%
    select(date, data_series) # Keep only the 2 columns we need
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}

create_federal_ui <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_ui) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_ui)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
      mutate(
      data_series = 
        # The coalesce turns NAS into zeroes
        coalesce(gftfpu, 0) + # Haver code for ui expansion
        coalesce(yptol, 0) # Haver code for wages lost assistance
    )  %>%
    # Hard coding one silly value here ¯\_(ツ)_/¯
    mutate_where(
      date == yearquarter('2021 Q4'),
      data_series = 11
      ) %>%
    select(date, data_series) # Keep only the 2 columns we need
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}



create_state_ui <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, state_ui) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = state_ui)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    mutate(
      data_series = 
        # This formula calculates state ui as `ui` - `federal_ui`, where
        # `federal_ui` = `ui_expansion + wages_lost assistance`
        yptu - # Haver code for ui
        # The coalesce turns NAS into zeroes
        coalesce(gftfpu, 0) - # Haver code for ui expansion
        coalesce(yptol, 0) # Haver code for wages lost assistance
    )  %>%
    # Since we set federal ui to equal 11, we're recalculating state ui in this
    # quarter to equal ui - 11
    mutate_where(
      date == yearquarter('2021 Q4'),
      data_series = yptu - 11
    ) %>%
    select(date, data_series) # Keep only the 2 columns we need
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_federal_subsidies <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_subsidies) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_subsidies)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    mutate(data_series = gfsub)  %>% # Haver code for federal subsidies
    select(date, data_series) # Keep only the 2 columns we need
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_federal_aid_to_small_businesses_arp <- function(
    national_accounts, 
    forecast, 
    historical_overrides,
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_aid_to_small_businesses_arp) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_aid_to_small_businesses_arp)
  
  # We don't actually have national accounts as a data series for this tibble.
  # So we populate it with a placeholder tibble spanning 1970 Q1 to 2034 Q3 with
  # 0s in each entry
  national_accounts <- 
    create_placeholder_nas(
      col_name = "data_series",
      start = "1970-01-01",
      end = "2034-07-01") %>%
    select(date, data_series) %>% # Keep only the 2 columns we need
  # Override historic entries of national_accounts using historical_overrides data
  mutate_where(
    date >= yearquarter('2020 Q3') & date <= current_quarter,
    data_series = historical_overrides$federal_aid_to_small_businesses_arp_override
  )
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_federal_other_direct_aid_arp <- function(
    national_accounts, 
    forecast, 
    historical_overrides,
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_other_direct_aid_arp) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_other_direct_aid_arp)
  
  # We don't actually have national accounts as a data series for this tibble.
  # So we populate it with a placeholder tibble spanning 1970 Q1 to 2034 Q3 with
  # 0s in each entry
  national_accounts <- 
    create_placeholder_nas(
      col_name = "data_series",
      start = "1970-01-01",
      end = "2034-07-01") %>%
    select(date, data_series) %>% # Keep only the 2 columns we need
    # Override historic entries of national_accounts using historical_overrides data
    mutate_where(
      date >= yearquarter('2020 Q3') & date <= current_quarter,
      data_series = historical_overrides$federal_other_direct_aid_arp_override
    )
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_federal_other_vulnerable_arp <- function(
    national_accounts, 
    forecast, 
    historical_overrides,
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_other_vulnerable_arp) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_other_vulnerable_arp)
  
  # We don't actually have national accounts as a data series for this tibble.
  # So we populate it with a placeholder tibble spanning 1970 Q1 to 2034 Q3 with
  # 0s in each entry
  national_accounts <- 
    create_placeholder_nas(
      col_name = "data_series",
      start = "1970-01-01",
      end = "2034-07-01") %>%
    select(date, data_series) %>% # Keep only the 2 columns we need
    # Override historic entries of national_accounts using historical_overrides data
    mutate_where(
      date >= yearquarter('2020 Q3') & date <= current_quarter,
      data_series = historical_overrides$federal_other_vulnerable_arp_override
    )
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_federal_student_loans <- function(
    national_accounts, 
    forecast, 
    historical_overrides,
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_student_loans) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_student_loans)
  
  # We don't actually have national accounts as a data series for this tibble.
  # So we populate it with a placeholder tibble spanning 1970 Q1 to 2034 Q3 with
  # 0s in each entry
  national_accounts <- 
    create_placeholder_nas(
      col_name = "data_series",
      start = "1970-01-01",
      end = "2034-07-01") %>%
    select(date, data_series) %>% # Keep only the 2 columns we need
    # Override historic entries of national_accounts using historical_overrides data
    mutate_where(
      date >= yearquarter('2020 Q3') & date <= current_quarter,
      data_series = historical_overrides$federal_student_loans_override
    )
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_state_subsidies <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  ## UH OH! WE ACTUALLY DO NOT HAVE STATE SUBSIDIES IN THE FORECAST SHEET.
  ## SOMEONE NEEDS TO ADD THIS. Without a forecast for state subsidies,
  ## we've been assuming that they are zero!
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    mutate(data_series = gssub)  %>% # Haver code for federal subsidies
    select(date, data_series) # Keep only the 2 columns we need
  
  # Merge the national accounts with the a data frame of NAs extending to 2034 Q3.
  result <- coalesce_join(national_accounts, placeholder_nas, by = 'date') %>% 
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_federal_health_outlays <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    # Our federal health outlays prediction is the sum of two other predictions
    mutate(data_series = medicare + medicaid_grants) %>%
    select(date, data_series)
  
  # Select columns of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Calculate federal health outlays as a sum of 2 series
    mutate(
      data_series = 
        # The coalesce turns NAS into zeroes
        coalesce(yptmr, 0) + # Haver code for medicare
        coalesce(gfeghdx, 0) # Haver code for medicaid grants
    )  %>%
    select(date, data_series) # Keep only the 2 columns we need
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


create_state_health_outlays <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    # Our state health outlays prediction is the difference of two other predictions
    mutate(data_series = medicaid - medicaid_grants) %>%
    select(date, data_series)
  
  # Select columns of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Calculate federal health outlays as a diference of 2 series
    mutate(
      data_series = 
        # The coalesce turns NAS into zeroes
        coalesce(yptmd, 0) - # Haver code for medicaid
        coalesce(gfeghdx, 0) # Haver code for medicaid grants
    )  %>%
    select(date, data_series) # Keep only the 2 columns we need
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}


# ---- Accessory-variables ----
# So, while the main variables use national accounts to construct them, these
# accessory variables use projections to construct them.
# Explain this in the bookdown file

# Deflators

create_federal_purchases_deflator_growth <- function(
    national_accounts, 
    projections, 
    deflator_overrides,
    placeholder_nas
) {
  # Select column of interest from the projections tibble
  projections <- projections %>% 
    # Divide federal purchases "gf" by real federal purchases "gfh" and calculate
    # the quarterly growth rate using the qgr function, equal to x/lag(x), then 
    # subtract 1.
    mutate(data_series = qgr((gf / gfh)) - 1) %>%
    select(date, data_series)
  
  # Select columns of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Haver code for federal purchases deflator is 'jgf'
    mutate(data_series = jgf_growth) %>%
    select(date, data_series)
  
  # Process the overrides data so they are ready for merging
  deflator_overrides <- deflator_overrides %>%
    filter(date > current_quarter & date <= max(date)) %>%
    # Rename the column of interest to data_series
    mutate(data_series = federal_purchases_deflator_growth_override) %>%
    select(date, data_series)
  
  # Merge the national accounts with the projection using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, projections, by = 'date') %>%
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(., placeholder_nas, by = "date") %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0))) %>%
    # Use deflator_overrides to overwrite select existing entries
    coalesce_join(deflator_overrides, ., by = "date") %>%
    # Reorder the entries chronologically
    arrange(date)

  return(result)
}

create_consumption_grants_deflator_growth <- function(
    national_accounts, 
    projections, 
    deflator_overrides,
    placeholder_nas
) {
  # Select column of interest from the projections tibble. We use the state purchases
  # deflator growth as our consumption grants deflator growth projection.
  projections <- projections %>% 
    # Divide state purchases "gs" by real state purchases "gsh" and calculate
    # the quarterly growth rate using the qgr function, equal to x/lag(x), then 
    # subtract 1.
    mutate(data_series = qgr((gs / gsh)) - 1) %>%
    select(date, data_series)
  
  # Select columns of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Haver code for federal purchases deflator is 'jgf'
    mutate(data_series = jgse_growth) %>%
    select(date, data_series)
  
  # Process the overrides data so they are ready for merging
  deflator_overrides <- deflator_overrides %>%
    filter(date > current_quarter & date <= max(date)) %>%
    # Rename the column of interest to data_series
    mutate(data_series = consumption_grants_deflator_growth_override) %>%
    select(date, data_series)
  
  # Merge the national accounts with the projection using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, projections, by = 'date') %>%
    # Merge with a data frame of NAs extending to 2034 Q3
    # NOTE DON"T FORGET TO CHANGE CREATE_PLACEHOLDER_NAS back
    coalesce_join(., create_placeholder_nas(), by = "date") %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0))) %>%
    # Use deflator_overrides to overwrite select existing entries
    coalesce_join(deflator_overrides, ., by = "date") %>%
    # Reorder the entries chronologically
    arrange(date)
  
  return(result)
}


create_investment_grants_deflator_growth <- function(
    national_accounts, 
    projections, 
    deflator_overrides,
    placeholder_nas
) {
  # Select column of interest from the projections tibble. We use the state purchases
  # deflator growth as our investment grants deflator growth projection.
  projections <- projections %>% 
    # Divide state purchases "gs" by real state purchases "gsh" and calculate
    # the quarterly growth rate using the qgr function, equal to x/lag(x), then 
    # subtract 1.
    mutate(data_series = qgr((gs / gsh)) - 1) %>%
    select(date, data_series)
  
  # Select columns of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Haver code for federal purchases deflator is 'jgf'
    mutate(data_series = jgsi_growth) %>%
    select(date, data_series)
  
  # Process the overrides data so they are ready for merging
  deflator_overrides <- deflator_overrides %>%
    filter(date > current_quarter & date <= max(date)) %>%
    # Rename the column of interest to data_series
    mutate(data_series = investment_grants_deflator_growth_override) %>%
    select(date, data_series)
  
  # Merge the national accounts with the projection using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, projections, by = 'date') %>%
    # Merge with a data frame of NAs extending to 2034 Q3
    # NOTE DON"T FORGET TO CHANGE CREATE_PLACEHOLDER_NAS back
    coalesce_join(., create_placeholder_nas(), by = "date") %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0))) %>%
    # Use deflator_overrides to overwrite select existing entries
    coalesce_join(deflator_overrides, ., by = "date") %>%
    # Reorder the entries chronologically
    arrange(date)
  
  return(result)
}

create_state_purchases_deflator_growth <- function(
    national_accounts, 
    projections, 
    deflator_overrides,
    placeholder_nas
) {
  # Select column of interest from the projections tibble
  projections <- projections %>% 
    # Divide state purchases "gs" by real state purchases "gsh" and calculate
    # the quarterly growth rate using the qgr function, equal to x/lag(x), then 
    # subtract 1.
    mutate(data_series = qgr((gs / gsh)) - 1) %>%
    select(date, data_series)
  
  # Select columns of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Haver code for state purchases deflator is 'jgs'
    mutate(data_series = jgs_growth) %>%
    select(date, data_series)
  
  # Process the overrides data so they are ready for merging
  deflator_overrides <- deflator_overrides %>%
    filter(date > current_quarter & date <= max(date)) %>%
    # Rename the column of interest to data_series
    mutate(data_series = state_purchases_deflator_growth_override) %>%
    select(date, data_series)
  
  # Merge the national accounts with the projection using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, projections, by = 'date') %>%
    # Merge with a data frame of NAs extending to 2034 Q3
    # NOTE DON"T FORGET TO CHANGE CREATE_PLACEHOLDER_NAS back
    coalesce_join(., create_placeholder_nas(), by = "date") %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0))) %>%
    # Use deflator_overrides to overwrite select existing entries
    coalesce_join(deflator_overrides, ., by = "date") %>%
    # Reorder the entries chronologically
    arrange(date)
  
  return(result)
}

create_consumption_deflator_growth <- function(
    national_accounts, 
    projections, 
    deflator_overrides,
    placeholder_nas
) {
  # Select column of interest from the projections tibble
  projections <- projections %>% 
    # Divide consumption "c" by real consumption "ch" and calculate
    # the quarterly growth rate using the qgr function, equal to x/lag(x), then 
    # subtract 1.
    mutate(data_series = qgr((c / ch)) - 1) %>%
    select(date, data_series)
  
  # Select columns of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Haver code for state purchases deflator is 'jgs'
    mutate(data_series = jc_growth) %>%
    select(date, data_series)
  
  # Process the overrides data so they are ready for merging
  deflator_overrides <- deflator_overrides %>%
    filter(date > current_quarter & date <= max(date)) %>%
    # Rename the column of interest to data_series
    mutate(data_series = consumption_deflator_growth_override) %>%
    select(date, data_series)
  
  # Merge the national accounts with the projection using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, projections, by = 'date') %>%
    # Merge with a data frame of NAs extending to 2034 Q3
    # NOTE DON"T FORGET TO CHANGE CREATE_PLACEHOLDER_NAS back
    coalesce_join(., placeholder_nas, by = "date") %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0))) %>%
    # Use deflator_overrides to overwrite select existing entries
    coalesce_join(deflator_overrides, ., by = "date") %>%
    # Reorder the entries chronologically
    arrange(date)
  
  return(result)
}


# Deflators

# This function is simple. We just take real potential GDP from projections and 
# calculate its growth rate. We then take real potential GDP from national accounts
# and calculate its growth rate. The two growth rates are combined into a data 
# series with national accounts predominating over projections.
create_real_potential_gdp_growth <- function(
    national_accounts, 
    projections, 
    placeholder_nas
) {
  # Select column of interest from the projections tibble
  projections1 <- projections %>% 
    # Extract Haver code "gdppothq" (real potential GDP) 
    mutate(data_series = q_g(gdppothq)) %>%
    select(date, data_series)
  
  # Select columns of interest from the national accounts tibble
  national_accounts1 <- national_accounts %>% 
    # Haver code for state purchases deflator is 'jgs'
    mutate(data_series = q_g(gdppothq)) %>%
    select(date, data_series)
  
  # Merge the national accounts with the projection using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts1, projections1, by = 'date') %>%
    # Merge with a data frame of NAs extending to 2034 Q3
    # NOTE DON"T FORGET TO CHANGE CREATE_PLACEHOLDER_NAS back
    coalesce_join(., placeholder_nas, by = "date") %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0))) %>%
    # Reorder the entries chronologically
    arrange(date)
  
  return(result)
}

create_gdp <- function(
  national_accounts, 
  projections, 
  placeholder_nas
) {
  # Redefine GDP and real GDP values in the future using CBO growth rates
  # Select column of interest from the projections tibble
  projections1 <- projections %>% 
    mutate(gdp_growth = q_g(gdp)) %>%
    # We only need the gdp growth rate from our projections
    select(date, gdp_growth) %>%
    # We need only the growth rates that represent future quarters
    filter(date > current_quarter)
  
  # Select columns of interest from the national accounts tibble
  national_accounts1 <- national_accounts %>% 
    # Haver code for GDP is 'gdp'
    select(date, gdp)
  # Define an index number for the current data point and end data point
  current_index <- which(national_accounts1$date == current_quarter)
  seed_gdp = national_accounts1$gdp[current_index]
  
  # Define new GDP projections by growing current GDP (seed) at CBO growth rates
  # using the cumulative_series() function
  new_gdp_projections <- cumulative_series(
    seed = seed_gdp,
    growth_rates = 1 + projections1$gdp_growth
  )
  
  # Define projections row as this adjusted forecast
  projections2 <- projections1 %>%
    mutate(gdp = new_gdp_projections) %>%
    select(date, gdp)
  
  # Merge the national accounts with the projection using the commonly named `gdp`
  # and `date` columns. 
  result <- coalesce_join(national_accounts1, projections2, by = 'date') %>%
    # Rename the gdp column to generic data_series
    rename(data_series = gdp) %>%
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(., placeholder_nas, by = "date") %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0))) %>%
    # Reorder the entries chronologically
    arrange(date)
  
  return(result)
}
# gdp <- projections$gdp
# # Extras
# date <- projections$date
# id <- projections$id
# recession <- projections$recession