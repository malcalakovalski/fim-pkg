# fiscal_impact_BETA.R
#
# This script runs the main FIM. It's the working replacement for fiscal_impact.R
# that will eventually substitute for the original. It includes chunk names  that
# are used in the technical walkthrough in docs/technical_documentation. Thus,
# it's essential to be careful when editing chunk names to avoid causing an error
# in docs/technical_documentation/index.Rmd

# This, for example, is a chunk name:
# ---- section-A.1-prep-for-update ----

Sys.setenv(TZ = 'UTC') # Set the default time zone to UTC (Coordinated Universal Time)

# Load packages
packages <- c(
  "tidyverse", "tsibble", "lubridate", "glue", 
  "TimTeaFan/dplyover", "zoo", "TTR", "fs", "gt", 
  "openxlsx", "snakecase", "rlang", "BrookingsInstitution/ggbrookings"
)
librarian::shelf(packages)

# Load all functions in package (?!?)
devtools::load_all() 

options(digits = 4) # Limit number of digits
options(scipen = 20)# Turn off scientific notation under 20 digits 

#are we running this after a cbo baseline and pre-bea update?
post_cbo_baseline<- FALSE
# Set the value of 'month_year' to the current month and year (in the format "mm-yyyy")
last_month_year <- glue('{format.Date(today() %m-% months(1), "%m")}-{year(today() %m-% months(1))}')
month_year <- glue('{format.Date(today() - 7, "%m")}-{year(today())}')
print(month_year)

# Calculate the current date minus 7 days
current_date <- today() - dweeks(1)
# Calculate the previous month date, handling wraparound (i.e. previous
# month to January ("01") is December ("12"), not month "0")
last_month_date <- current_date %m-% months(1)
# Extract and format the month as a two-digit string
last_month_2digit <- sprintf("%02d", month(last_month_date))
# Extract the year from the last_month_date
last_year <- year(last_month_date)
# Create last_month_year string for file naming
last_month_year <- glue('{last_month_2digit}-{last_year}')

print(last_month_year)

# ---- section-A.2-create-empty-directories ----

#setting our reference period to be the post-cbo files if we've already produced
# fim output incorporating the cbo update
if(file.exists(glue('results/{month_year}-post-cbo'))){
  last_month_year<- glue('{month_year}-post-cbo')
}

# Create folder for current update in the results directory
dir_create(glue('results/{month_year}')) 
# Folder to store forecast sheet from current update
dir_create(glue('results/{month_year}/input_data')) 
# Beta folder for Lorae's refactored results
dir_create(glue('results/{month_year}/beta'))

# Copy the file 'forecast.xlsx' from the 'data' directory to the 'input_data' directory
# This is the copy we keep for the current update
file_copy(
  path = 'data/forecast.xlsx', 
  new_path = glue('results/{month_year}/input_data/forecast_{month_year}.xlsx'), 
  overwrite = TRUE
)

# ---- section-B-test-data-import ----
# Source the module in the src directory containing the functions which import
# data
source("src/data_import.R")

## Read in data sources to be combined
projections <- import_projections()
national_accounts <- import_national_accounts()
forecast <- import_forecast()
historical_overrides <- import_historical_overrides()

## Calculate what the current quarter is using the date from historical overrides
current_quarter <- historical_overrides %>% slice_max(date) %>% pull(date)

# Source the module that creates the test data columns used in the FIM
source("src/data_cleaning.R")

# Run the functions defined in src/data_cleaning.R to produce the FIM data columns
federal_purchases_test <- create_federal_purchases(
  national_accounts, 
  forecast, 
  create_placeholder_nas()
)

consumption_grants_test <- create_consumption_grants(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)

investment_grants_test <- create_investment_grants(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)

state_purchases_test <- create_state_purchases(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

federal_non_corporate_taxes_test <- create_federal_non_corporate_taxes(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

state_non_corporate_taxes_test <- create_state_non_corporate_taxes(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

federal_corporate_taxes_test <- create_federal_corporate_taxes(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)

supply_side_ira_test <- create_supply_side_ira(
  forecast,
  historical_overrides,
  create_placeholder_nas()
)


state_corporate_taxes_test <- create_state_corporate_taxes(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)

federal_social_benefits_test <- create_federal_social_benefits(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)

state_social_benefits_test <- create_state_social_benefits(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

rebate_checks_test <- create_rebate_checks(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

rebate_checks_arp_test <- create_rebate_checks_arp(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

federal_ui_test <- create_federal_ui(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

state_ui_test <- create_state_ui(
  national_accounts,
  forecast,
  create_placeholder_nas()
)


federal_subsidies_test <- create_federal_subsidies(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

federal_aid_to_small_businesses_arp_test <- create_federal_aid_to_small_businesses_arp(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)


federal_other_direct_aid_arp_test <- create_federal_other_direct_aid_arp(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)


federal_other_vulnerable_arp_test <- create_federal_other_vulnerable_arp(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)


federal_student_loans_test <- create_federal_student_loans(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)


state_subsidies_test <- create_state_subsidies(
  national_accounts,
  forecast,
  create_placeholder_nas()
)


federal_health_outlays_test <- create_federal_health_outlays(
  national_accounts,
  forecast,
  create_placeholder_nas()
)


state_health_outlays_test <- create_state_health_outlays(
  national_accounts,
  forecast,
  create_placeholder_nas()
)


# ---- section-B.0-read-raw-rds-data ----

# Load in national accounts. This file is rewritten each time data-raw/haver-pull.R
# is run.
fim::national_accounts # this is the literal df
load("data/national_accounts.rda") # this loads in a df named national_accounts

# Load in projections. This file is rewritten each time data-raw/haver-pull.R
# is run.
fim::projections # this is the literal df
load("data/projections.rda") # this loads in a df named projections


# ---- section-B.1-read-overrides ----

# Read in historical overrides from data/forecast.xlsx
# Since BEA put all CARES act grants to S&L in Q2 2020 we need to
# override the historical data and spread it out based on our best guess
# for when the money was spent.
historical_overrides <- readxl::read_xlsx('data/forecast.xlsx',
                                          sheet = 'historical overrides') %>% # Read in historical_overrides
  select(-name) %>% # Remove longer name since we don't need it
  pivot_longer(-variable,
               names_to = 'date') %>% # Reshape so that variables are columns and dates are rows
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  mutate(date = yearquarter(date))

# Read in deflator overrides from data/forecast.xlsx
deflator_overrides <- readxl::read_xlsx('data/forecast.xlsx',
                                        sheet = 'deflators_override') %>% # Read in overrides for deflators
  select(-name) %>% # Remove longer name since we don't need it
  pivot_longer(-variable,
               names_to = 'date') %>% # Reshape so that variables are columns and dates are rows
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  mutate(date = yearquarter(date))

# ---- section-B.2-set-current-quarter ----
# TODO: This current quarter should be calculated at the top, for the entirety
# of the FIM, not buried down here.
# Save current quarter for later
current_quarter <- historical_overrides %>% slice_max(date) %>% pull(date) 

# Quarterly Federal Purchases Deflator Growth 
federal_purchases_deflator_growth_test <- create_federal_purchases_deflator_growth(
  national_accounts,
  projections,
  deflator_overrides,
  create_placeholder_nas()
)

# Annualized Federal Purchases Deflator Growth 
federal_purchases_deflator_growth_annualized_test <- create_annualized_growth(
  x= federal_purchases_deflator_growth_test)

# Quarterly Consumption Grants Deflator Growth 
consumption_grants_deflator_growth_test <- create_consumption_grants_deflator_growth(
  national_accounts,
  projections,
  deflator_overrides,
  create_placeholder_nas()
)

# Annualized Consumption Grants Deflator Growth 
consumption_grants_deflator_growth_annualized_test <- create_annualized_growth(
  x= consumption_grants_deflator_growth_test)

# Quarterly Investment Grants Deflator Growth 
investment_grants_deflator_growth_test <- create_investment_grants_deflator_growth(
  national_accounts,
  projections,
  deflator_overrides,
  create_placeholder_nas()
)

# Annualized Investment Grants Deflator Growth
investment_grants_deflator_growth_annualized_test <- create_annualized_growth(
  x= investment_grants_deflator_growth_test)

# Quarterly State Purchases Deflator Growth 
state_purchases_deflator_growth_test <- create_state_purchases_deflator_growth(
  national_accounts,
  projections,
  deflator_overrides,
  create_placeholder_nas()
)

# Annualized State Purchases Deflator Growth 
state_purchases_deflator_growth_annualized_test <- create_annualized_growth(
  x= state_purchases_deflator_growth_test)

# Quarterly Consumption Deflator Growth
consumption_deflator_growth_test <- create_consumption_deflator_growth(
  national_accounts,
  projections,
  deflator_overrides,
  create_placeholder_nas()
)

# Annualized Consumption Deflator Growth 
consumption_deflator_growth_annualized_test <- create_annualized_growth(
  x= consumption_deflator_growth_test)

# Quarterly Real Potential GDP Growth 
real_potential_gdp_growth_test <- create_real_potential_gdp_growth(
  national_accounts,
  projections,
  create_placeholder_nas()
)

# Annualized Real Potential GDP Growth 
real_potential_gdp_growth_annualized_test <- create_annualized_growth(
  x= real_potential_gdp_growth_test)

# GDP 
gdp_test <- create_gdp(
  national_accounts,
  projections,
  create_placeholder_nas()
)

# Consumption
consumption_test <- create_consumption(
  national_accounts,
  projections,
  create_placeholder_nas()
)

# ---- section-B.3-initial-import-projections ----

projections <- projections %>% 
  # Rename the variables from their Haver codes
  transmute(
    id,
    date,
    gdp,
    real_potential_gdp = gdppothq,
    consumption = c,
    real_consumption = ch,
    federal_purchases = gf,
    real_federal_purchases = gfh,
    state_purchases = gs,
    real_state_purchases = gsh
  )

projections <- projections %>%
  # Implicit price deflators
  mutate(
    # Why do we calculate these values and not get them from Haver instead?
    federal_purchases_deflator =  federal_purchases/real_federal_purchases, 
    state_purchases_deflator = state_purchases/real_state_purchases,
    consumption_deflator = consumption/real_consumption
  ) %>%
  # Growth rates
  mutate(
    across(
      .cols = c(
        "gdp", 
        "real_potential_gdp", 
        "federal_purchases_deflator", 
        "state_purchases_deflator", 
        "consumption_deflator"),
      #"jgse"),
      # Calculate quarterly growth rate using qgr() function, equal to x/lag(x), 
      # then subtract 1.
      .fns = ~ qgr(.) - 1,
      .names = "{.col}_growth"
    ) 
  ) %>%
  # Turn date into time series
  mutate(date = tsibble::yearquarter(date)) %>%
  # reorder the id column before the date column
  relocate(id, .before = date) %>%
  # convert the projections df into a tsibble data frame type
  tsibble::as_tsibble(key = id, index = date) %>%
  # TODO: as you can see from the select function, many columns are not kept.
  # Perhaps the code can be refactored to exclude the data processing steps 
  # in the first place.
  select(
    -real_federal_purchases, # we don't need anymore, as we created the _deflator_growth var already
    -real_state_purchases, # we don't need anymore, as we created the _deflator_growth var already
    -federal_purchases, # we don't need anymore, as we created the _deflator_growth var already
    -state_purchases, # we don't need anymore, as we created the _deflator_growth var already
    -federal_purchases_deflator, # we don't need anymore, as we created the _deflator_growth var already
    -state_purchases_deflator, # we don't need anymore, as we created the _deflator_growth var already
    -consumption_deflator, # we don't need anymore, as we created the _deflator_growth var already
    -consumption, # we don't need anymore, as we created the _deflator_growth var already
    -real_consumption # we don't need anymore, as we created the _deflator_growth var already
  )

# ---- section-B.4-initial-import-national-accounts ----

national_accounts <- national_accounts %>%
  # Let's rename these 90 variables to something we can understand
  transmute(
    id,
    date,
    gdp,
    medicare = yptmr,
    medicaid = yptmd,
    ui = yptu,
    social_benefits = gtfp,
    federal_purchases = gf,
    state_purchases = gs,
    federal_personal_taxes =  gfrpt,
    federal_production_taxes = gfrpri,
    federal_corporate_taxes = gfrcp,
    federal_payroll_taxes = gfrs,
    federal_social_benefits = gftfp,
    gross_consumption_grants = gfeg,
    state_personal_taxes =  gsrpt,
    state_production_taxes = gsrpri,
    state_corporate_taxes = gsrcp,
    state_payroll_taxes = gsrs,
    state_social_benefits = gstfp,
    medicaid_grants = gfeghdx,
    investment_grants = gfeigx,
    federal_subsidies = gfsub,
    state_subsidies = gssub,
    rebate_checks = gftfpe,
    nonprofit_provider_relief_fund = gftfpv, 
    ui_expansion = gftfpu,
    wages_lost_assistance = coalesce(yptol, 0), # idk what this does
    real_potential_gdp = gdppothq,
    recession = recessq,
    consumption_deflator_growth = jc_growth,
    federal_purchases_deflator_growth = jgf_growth,
    state_purchases_deflator_growth = jgs_growth,
    consumption_grants_deflator_growth = jgse_growth,
    investment_grants_deflator_growth = jgsi_growth
  )

# ---- section-B.5-join-national-accounts-to-projections ----
usna1 <- coalesce_join(x = national_accounts,
                       y = projections,
                       by = 'date') %>%
  as_tsibble(key = id, index = date)

# ---- section-B.6-forecast-gdp-using-cbo ----

#### Redefine GDP and real GDP values in the future using CBO growth rates

# Define an index number for the current data point and end data point
current_index <- which(usna1$date == current_quarter)
end_index <- nrow(usna1)

# Define new GDP projections by growing current GDP (seed) at CBO growth rates
# using the cumulative_series() function
new_gdp_projections <- cumulative_series(
  seed = usna1$gdp[current_index],
  growth_rates = 1 + usna1$gdp_growth[(current_index + 1):end_index]
)

# Assign new GDP and real GDP projections back to the `gdp` series in the USNA 
# dataframe
usna2 <- usna1 
usna2$gdp[(current_index + 1):end_index] <- new_gdp_projections

usna2 <- usna2 %>%
  # Delete the gdp_growth variable, which is no longer needed
  select(
    -gdp_growth,
  ) %>%
  as_tsibble(key = id, index = date) %>% # Specifies the time series structure of the data, with the id column as the key and the date column as the index.
  
  mutate_where(id == 'historical',  # Calculate GDP growth for data 
               real_potential_gdp_growth = q_g(real_potential_gdp))

usna3 <- usna2 %>%
  #Define FIM variables 
  mutate( 
    # Net out unemployment insurance, rebate checks, and Medicare to apply different MPC's
    federal_ui = coalesce(ui_expansion, 0) +  wages_lost_assistance,
    state_ui = ui - federal_ui,
    #state_ui = ui - federal_ui,
    # replace NAs with 0 to avoid errors in later subtraction
    ui = coalesce(ui, 0),
    rebate_checks = coalesce(rebate_checks, 0),
    nonprofit_provider_relief_fund = coalesce(nonprofit_provider_relief_fund, 0),
    federal_social_benefits = federal_social_benefits - ui - rebate_checks - medicare - nonprofit_provider_relief_fund,
    state_social_benefits = state_social_benefits - medicaid,
    consumption_grants = gross_consumption_grants - medicaid_grants,
  ) %>% 
  
  mutate(rebate_checks_arp = if_else(date == yearquarter("2021 Q1"), #hardcoding arp rebate checks for one period
                                     1348.1,
                                     0)) %>%
  
  #Set future periods to NA in these time series, allowing them to be overridden
  # by subsequent merges
  mutate_where(id == 'projection',
               rebate_checks_arp = NA,
               federal_ui = NA,
               state_ui = NA) %>%
  
  ##Adjusting data in 2021 because of arp(?)
  mutate_where(date == yearquarter('2021 Q1'),
               rebate_checks = rebate_checks - rebate_checks_arp,
               federal_social_benefits = federal_social_benefits + 203
  ) %>% 
  mutate_where(date == yearquarter("2021 Q4"),
               rebate_checks_arp = 14.2,
               rebate_checks = 0) %>% 
  mutate(consumption_grants = gross_consumption_grants - medicaid_grants,
         
         # Aggregate taxes
         federal_non_corporate_taxes = federal_personal_taxes + federal_production_taxes + federal_payroll_taxes,
         state_non_corporate_taxes = state_personal_taxes + state_production_taxes + state_payroll_taxes) %>% 
  
  ##Set the grants deflator the same as state purchases deflator (the same is done in the forecast/deflators sheet)
  mutate_where(id == 'projection',
               consumption_grants_deflator_growth = state_purchases_deflator_growth,
               investment_grants_deflator_growth = state_purchases_deflator_growth) %>% 
  
  #Overriding historical consumption and investment grant 
  # I think we override this twice???
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               consumption_grants = historical_overrides$consumption_grants_override) %>% 
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter, 
               investment_grants = historical_overrides$investment_grants_override) %>%
  
  #For the full period of the forecast (8 quarters out), replace CBO deflators with the ones from
  #the deflator overrides sheet
  mutate_where(date>current_quarter & date<=max(deflator_overrides$date), 
               consumption_deflator_growth = deflator_overrides$consumption_deflator_growth_override,
               federal_purchases_deflator_growth =deflator_overrides$federal_purchases_deflator_growth_override,
               state_purchases_deflator_growth = deflator_overrides$state_purchases_deflator_growth_override,
               consumption_grants_deflator_growth = deflator_overrides$consumption_grants_deflator_growth_override,
               investment_grants_deflator_growth = deflator_overrides$investment_grants_deflator_growth_override
  ) %>%
  # delete unneeded tax vars which were already rolled into federal non corporate taxes
  # and state non corporate taxes
  select(
    -federal_personal_taxes,
    -federal_production_taxes,
    -federal_payroll_taxes,
    -state_personal_taxes,
    -state_production_taxes,
    -state_payroll_taxes
  )


# Redefine usna to be integrated back into the FIM
usna <- usna3


# Section C: Forecast ----------------------------------------------------------------
forecast <- # Read in sheet with our forecasted values from the data folder
  readxl::read_xlsx('data/forecast.xlsx',
                    sheet = 'forecast') %>% 
  select(-name) %>% #Remove the 'name' column from the data.
  pivot_longer(-variable,
               names_to = 'date') %>%  #reshape the data 
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  mutate(date = yearquarter(date)) %>% #convert date to year-quarter format 
  tsibble::as_tsibble(index = date)

# Store forecast sheet for Shiny App 
forecast_shiny <- 
  readxl::read_xlsx('data/forecast.xlsx', 
                    sheet = 'forecast')
openxlsx::write.xlsx(forecast_shiny, file = glue('shiny/cache/forecast.xlsx'), overwrite = TRUE)
rm(forecast_shiny)

# Remove all the unneeded columns from USNA before merging
usna <- usna %>%
  select(
    -real_potential_gdp,
    -gross_consumption_grants,
    -ui_expansion,
    -wages_lost_assistance,
    -nonprofit_provider_relief_fund
  )

save(usna, file = 'shiny/cache/usna.rda')

projections <- # Merge forecast w BEA + CBO on the 'date' column, 
  #filling in NA values with the corresponding value from the other data frame
  coalesce_join(usna, forecast, by = 'date') %>%  
  
  mutate( # Coalesce NA's to 0 for all numeric values 
    across(where(is.numeric),
           ~ coalesce(.x, 0))) %>%
  
  #Define FIM variables 
  mutate(
    federal_health_outlays = medicare + medicaid_grants,
    state_health_outlays = medicaid - medicaid_grants
  ) %>% 
  
  #apply historical_overrides for ARP 
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               federal_other_direct_aid_arp = historical_overrides$federal_other_direct_aid_arp_override,
               federal_other_vulnerable_arp = historical_overrides$federal_other_vulnerable_arp_override,
               federal_social_benefits = historical_overrides$federal_social_benefits_override,
               federal_aid_to_small_businesses_arp = historical_overrides$federal_aid_to_small_businesses_arp_override) %>% 
  mutate_where(date == current_quarter & is.na(federal_corporate_taxes) & is.na(state_corporate_taxes),
               federal_corporate_taxes = tail(historical_overrides$federal_corporate_taxes_override, n = 1),
               state_corporate_taxes = tail(historical_overrides$state_corporate_taxes_override, n = 1)) %>% 
  mutate_where(date == yearquarter("2021 Q1"),
               federal_social_benefits = federal_social_benefits + 203) %>% 
  # FIXME: Figure out why wrong number was pulled from Haver (like 400)
  mutate_where(date == yearquarter('2021 Q4'),
               federal_ui = 11, 
               state_ui = ui - federal_ui) %>%
  #apply historical_overrides for Supply Side IRA
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               supply_side_ira = historical_overrides$supply_side_ira_override) %>%
  #apply historical_overrides for Federal Student Loans
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               federal_student_loans = historical_overrides$federal_student_loans_override)

# The `projections` data frame, at this point, contains all of the data we need
# in order to calculate the FIM. We streamline it to remove all the unneeded columns.
projections <- projections %>%
  select(
    -medicare,  # Used in calculation but no longer needed
    -medicaid_grants,  # Used in calculation but no longer needed
    -medicaid,  # Used in calculation but no longer needed
    -ui # Used in calculation but no longer needed
  )

######################################################################################
# This is the point where we go from generating our data inputs to actually calculating the FIM
######################################################################################

# This script defines the 33 input variables used in the FIM. It assumes that the 
# projections data frame is saved in memory from the section above having already
# been run.
source("src/define_inputs.R")

# Next, we source essential functions we need to calculate the FIM in this section.
# All of these modules contain nothing but functions. No actual code is executed
# when you source them. Instead, the code is executed in this script.
source("src/contributions.R")

# Another type of variable we need is MPC matrices. If you read the documentation
# in `src/mpc_lorae.R`, you'll develop a clearer understanding of how these 
# matrices produce an MPC operation. We cache these matrices so that they do not 
# need to be regenerated each time the code is run. Instead, in the future, we'll
# only rebuild these matrices when MPC inputs are changed using an "observer" 
# design pattern. This will save us a lot of computing time.
# 

### APPLY MPCS FOR TAXES AND TRANSFERS ########################################

# APPLY MPCS TO TAXES

# Federal Non-Corporate Taxes
post_mpc_federal_non_corporate_taxes <- mpc(x = federal_non_corporate_taxes_test$data_series, 
                                            mpc = readRDS("cache/mpc_matrices/federal_non_corporate_taxes.rds"))

# State Non-Corporate Taxes
post_mpc_state_non_corporate_taxes <- mpc(x = state_non_corporate_taxes_test$data_series, 
                                          mpc = readRDS("cache/mpc_matrices/state_non_corporate_taxes.rds"))

# Federal Corporate Taxes
post_mpc_federal_corporate_taxes <- mpc(x = federal_corporate_taxes_test$data_series, 
                                        mpc = readRDS("cache/mpc_matrices/federal_corporate_taxes.rds"))

# State Corporate Taxes
post_mpc_state_corporate_taxes <- mpc(x = state_corporate_taxes_test$data_series, 
                                      mpc = readRDS("cache/mpc_matrices/state_corporate_taxes.rds"))

# Supply Side IRA (no MPC)
supply_side_ira <- as.matrix(supply_side_ira_test$data_series)


# APPLY MPCS TO TRANSFERS #
# Federal Social Benefits 
post_mpc_federal_social_benefits <- mpc(x = federal_social_benefits_test$data_series, 
                                        mpc = readRDS("cache/mpc_matrices/federal_social_benefits.rds"))

# State Social Benefits
post_mpc_state_social_benefits <- mpc(x = state_social_benefits_test$data_series, 
                                      mpc = readRDS("cache/mpc_matrices/state_social_benefits.rds"))

# Rebate Checks 
post_mpc_rebate_checks <- mpc(x = rebate_checks_test$data_series, 
                              mpc = readRDS("cache/mpc_matrices/rebate_checks.rds"))

# Rebate Checks ARP 
post_mpc_rebate_checks_arp <- mpc(x = rebate_checks_arp_test$data_series, 
                                  mpc = readRDS("cache/mpc_matrices/rebate_checks_arp.rds"))

# Federal UI 
post_mpc_federal_ui <- mpc(x = federal_ui_test$data_series, 
                           mpc = readRDS("cache/mpc_matrices/federal_ui.rds"))

# State UI 
post_mpc_state_ui <- mpc(x = state_ui_test$data_series, 
                         mpc = readRDS("cache/mpc_matrices/state_ui.rds"))

# Federal Subsidies
post_mpc_federal_subsidies <- mpc(x = federal_subsidies_test$data_series, 
                                  mpc = readRDS("cache/mpc_matrices/federal_subsidies.rds"))

# Federal Aid to Small Businesses ARP 
post_mpc_federal_aid_to_small_businesses_arp <- mpc(x = federal_aid_to_small_businesses_arp_test$data_series, 
                                                    mpc = readRDS("cache/mpc_matrices/federal_aid_to_small_businesses_arp.rds"))

# Federal Other Direct Aid ARP 
post_mpc_federal_other_direct_aid_arp <- mpc(x = federal_other_direct_aid_arp_test$data_series, 
                                             mpc = readRDS("cache/mpc_matrices/federal_other_direct_aid_arp.rds"))

# Federal Other Vulnerable ARP 
post_mpc_federal_other_vulnerable_arp <- mpc(x = federal_other_vulnerable_arp_test$data_series, 
                                             mpc = readRDS("cache/mpc_matrices/federal_other_vulnerable_arp.rds"))

# Federal Student Loans 
post_mpc_federal_student_loans <- mpc(x = federal_student_loans_test$data_series,
                                      mpc = readRDS("cache/mpc_matrices/federal_student_loans.rds"))

# State Subsidies 
post_mpc_state_subsidies <- mpc(x = state_subsidies_test$data_series, 
                                mpc = readRDS("cache/mpc_matrices/state_subsidies.rds"))

# Federal Health Outlays 
post_mpc_federal_health_outlays <- mpc(x = federal_health_outlays_test$data_series,
                                       mpc = readRDS("cache/mpc_matrices/federal_health_outlays.rds"))

# State Health Outlays 
post_mpc_state_health_outlays <- mpc(x = state_health_outlays_test$data_series, 
                                     mpc = readRDS("cache/mpc_matrices/state_health_outlays.rds"))

### CREATE FEDERAL PURCHASES #####
federal_test <- data.frame(date = gdp_test$date, 
                           data_series = federal_purchases_test$data_series + 
                             consumption_grants_test$data_series + 
                             investment_grants_test$data_series)

### CREATE STATE PURCHASES #####
state_test <- data.frame(date = gdp_test$date, 
                         data_series = state_purchases_test$data_series -
                           consumption_grants_test$data_series -
                           investment_grants_test$data_series
                           )


#### CREATE TAXES #####
taxes_test <- data.frame(date = date, data_series = post_mpc_federal_non_corporate_taxes + 
                           post_mpc_state_non_corporate_taxes + post_mpc_federal_corporate_taxes + 
                           post_mpc_state_corporate_taxes + supply_side_ira) 

#### CREATE TRANSFERS #####

transfers_test <- data.frame(date = date, data_series = post_mpc_federal_social_benefits + post_mpc_state_social_benefits +
                               post_mpc_rebate_checks + post_mpc_rebate_checks_arp + 
                               post_mpc_federal_ui + post_mpc_state_ui + 
                               post_mpc_federal_subsidies + post_mpc_federal_aid_to_small_businesses_arp + 
                               post_mpc_federal_other_direct_aid_arp + post_mpc_federal_other_vulnerable_arp  + 
                               post_mpc_federal_student_loans + post_mpc_state_subsidies +
                               post_mpc_federal_health_outlays + post_mpc_state_health_outlays)

#### SUM TAXES AND TRANSFERS ####

taxes_transfers_test <- data.frame(date = date, 
                                   data_series = taxes_test$data_series + transfers_test$data_series)



#######################################################
#               CALCULATE THE FIM                     #
#######################################################

# Federal Purchases Contribution (NIPA Consistent)
federal_purchases_contribution <- contribution_purchases(
  x = federal_purchases_test$data_series, # Using the new test version
  dg = federal_purchases_deflator_growth_test$data_series, # Using the new test version
  rpgg = real_potential_gdp_growth_annualized_test$data_series, # Using the new test version
  gdp = gdp_test$data_series # Using the new test version
)

# State Purchases Contribution (NIPA Consistent)
state_purchases_contribution <- contribution_purchases(
  x = state_purchases_test$data_series, # Using the new test version
  dg = state_purchases_deflator_growth_test$data_series, # Using the new test version
  rpgg = real_potential_gdp_growth_annualized_test$data_series, # Using the new test version
  gdp = gdp_test$data_series # Using the new test version
) 
  

#### FIM FOR TAXES AND TRANSFERS #####
# Generate Counterfactual Taxes and Transfers
counterfactual_consumption <- t_counterfactual(
  x = taxes_transfers_test$data_series, 
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series
)
 
minus_neutral <- (consumption_test$data_series/lag(consumption_test$data_series))^4 - 
  (counterfactual_consumption/lag(consumption_test$data_series))^4

scale_to_gdp <- minus_neutral*(lag(consumption_test$data_series)/lag(gdp_test$data_series))

consumption_contribution <- scale_to_gdp*100

### AGGREGATE contributions ########################################
federal_contribution <- federal_purchases_contribution

state_contribution <- state_purchases_contribution

fiscal_impact_measure <-
  (federal_contribution +
  state_contribution +
  consumption_contribution) 

# Replace NAs with Zeros
fiscal_impact_measure <- replace(fiscal_impact_measure, 
                                 is.na(fiscal_impact_measure), 0)
  
# Calculate Four Quarter Moving Average
fiscal_impact_4q_ma <- fiscal_impact_measure %>%
  SMA(zoo::na.locf(., na.rm = F), n=4)

# Combine all the inputs into a data frame
inputs_df <- data.frame(
  date,
  id,
  recession,
  federal_purchases_deflator_growth,
  consumption_grants_deflator_growth,
  investment_grants_deflator_growth,
  state_purchases_deflator_growth,
  consumption_deflator_growth,
  real_potential_gdp_growth,
  gdp,
  federal_purchases,
  consumption_grants,
  investment_grants,
  state_purchases,
  federal_non_corporate_taxes,
  state_non_corporate_taxes,
  federal_corporate_taxes,
  supply_side_ira,
  state_corporate_taxes,
  federal_social_benefits,
  state_social_benefits,
  rebate_checks,
  rebate_checks_arp,
  federal_ui,
  state_ui,
  federal_subsidies,
  federal_aid_to_small_businesses_arp,
  federal_other_direct_aid_arp,
  federal_other_vulnerable_arp,
  federal_student_loans,
  state_subsidies,
  federal_health_outlays,
  state_health_outlays
) %>%
  as_tsibble(index = date)

# Combine all the contributions into a data frame
contributions_df <- data.frame(
  date,
  id,
  recession,
  federal_contribution,
  state_contribution,
  consumption_contribution,
  fiscal_impact_measure,
  fiscal_impact_4q_ma
) %>%
  as_tsibble(index = date)


# Write the contributions and inputs to an Excel file in results/{month_year}/beta
# TODO: This code only works if the beta/ directory already exists. 
openxlsx::write.xlsx(contributions_df, file = glue('results/{month_year}/beta/contributions-{month_year}.xlsx'), overwrite = TRUE)
openxlsx::write.xlsx(inputs_df, file = glue('results/{month_year}/beta/inputs-{month_year}.xlsx'), overwrite = TRUE)

write_rds(contributions_df, file = 'data/contributions.rds')
usethis::use_data(contributions_df, overwrite = TRUE)

# Section F: Web materials  -------------------------------------------------------------

# Generate interactive data frame from contributions
interactive <- contributions_df %>% 
  # Filter rows of contributions by date, keeping only those between 1999 Q4 and
  # current quarter + 8
  filter_index('1999 Q4' ~ as.character(current_quarter + 8)) %>% 
  # Select only specific columns
  select(date, 
         impact = fiscal_impact_4q_ma,
         recession,
         total = fiscal_impact_measure,
         federal = federal_contribution,
         state_local = state_contribution,
         consumption = consumption_contribution,
         projection = id
         ) %>% 
  # Recode `recession` and `projection` variables to 0 and 1 binaries
  mutate(recession = recode(recession, `-1` = 0),
         recession = replace_na(recession, 0),
         projection = recode(projection, historical = 0, projection = 1)
         ) %>%
  # Split date column into year and quarter columns
  separate(date, c('year', 'quarter'))

# Write interactive data frame to CSV file
readr::write_csv(interactive,  file = glue('results/{month_year}/beta/interactive-{month_year}.csv'))

# Make HTML FIM graphs for website and email. Save as FIM/Fiscal-Impact.html
rmarkdown::render('Fiscal-Impact.Rmd',
                  # Render R Markdown document to PDF file
                  output_file = 'Fiscal-Impact.html',
                  clean = TRUE,
                  params = list(start = yearquarter('1999 Q4'), end = current_quarter + 8))

# Copy FIM/Fiscal-Impact.html graphs to the results/month-year/beta folder
file_copy(path = 'Fiscal-Impact.html',
          new_path = glue('results/{month_year}/beta/Fiscal-Impact-{month_year}.html'),
          overwrite = TRUE)



# Get update comparison html file 
source("scripts/index_temp.R")

rmarkdown::render(input = 'update-comparison-markdown.Rmd',
                  output_file = glue('results/{month_year}/beta/update-comparison-{month_year}'),
                  clean = TRUE)

