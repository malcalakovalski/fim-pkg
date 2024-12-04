##############
# get_mpcs.R #
##############

# This script pulls in the mpcs that we store as matrices in 'fim/cache', formats them as one-column data frames, and then outputs them in 
# in an Excel file contained in the shiny folder. We use these mpcs to generate a spreadsheet that interactive users can edit with their own MPC assumptions. 

# Load packages
packages <- c(
  "tidyverse", "tsibble", "lubridate", "glue", 
  "TimTeaFan/dplyover", "zoo", "TTR", "fs", "gt", 
  "openxlsx", "snakecase", "rlang", "BrookingsInstitution/ggbrookings"
)
librarian::shelf(packages)

# Load all functions in package
devtools::load_all() 


# Read in all mpc matrices
federal_non_corporate_taxes_mpc <- readRDS("cache/mpc_matrices/federal_non_corporate_taxes.rds")
state_non_corporate_taxes_mpc <- readRDS("cache/mpc_matrices/state_non_corporate_taxes.rds")
federal_corporate_taxes_mpc <- readRDS("cache/mpc_matrices/federal_corporate_taxes.rds")
state_corporate_taxes_mpc <- readRDS("cache/mpc_matrices/state_corporate_taxes.rds")
federal_social_benefits_mpc <- readRDS("cache/mpc_matrices/federal_social_benefits.rds")
state_social_benefits_mpc <- readRDS("cache/mpc_matrices/state_social_benefits.rds")
rebate_checks_mpc <- readRDS("cache/mpc_matrices/rebate_checks.rds")
rebate_checks_arp_mpc <- readRDS("cache/mpc_matrices/rebate_checks_arp.rds")
federal_ui_mpc <- readRDS("cache/mpc_matrices/federal_ui.rds")
state_ui_mpc <- readRDS("cache/mpc_matrices/state_ui.rds")
federal_subsidies_mpc <- readRDS("cache/mpc_matrices/federal_subsidies.rds")
federal_aid_to_small_businesses_arp_mpc <- readRDS("cache/mpc_matrices/federal_aid_to_small_businesses_arp.rds")
federal_other_direct_aid_arp_mpc <- readRDS("cache/mpc_matrices/federal_other_direct_aid_arp.rds") 
federal_other_vulnerable_arp_mpc <- readRDS("cache/mpc_matrices/federal_other_vulnerable_arp.rds")  
federal_student_loans_mpc <- readRDS("cache/mpc_matrices/federal_student_loans.rds")
state_subsidies_mpc <- readRDS("cache/mpc_matrices/state_subsidies.rds")
federal_health_outlays_mpc <- readRDS("cache/mpc_matrices/federal_health_outlays.rds")
state_health_outlays_mpc <- readRDS("cache/mpc_matrices/state_health_outlays.rds")

# Extract the first column 
federal_non_corporate_taxes_mpc <- federal_non_corporate_taxes_mpc[1:17,1]
state_non_corporate_taxes_mpc <- state_non_corporate_taxes_mpc[1:17,1]
federal_corporate_taxes_mpc <- federal_corporate_taxes_mpc[1:17,1]
state_corporate_taxes_mpc <- state_corporate_taxes_mpc[1:17,1]
federal_social_benefits_mpc <- federal_social_benefits_mpc[1:17,1]
state_social_benefits_mpc <- state_social_benefits_mpc[1:17,1]
rebate_checks_mpc <- rebate_checks_mpc[1:17,1]
rebate_checks_arp_mpc <- rebate_checks_arp_mpc[1:17,1]
federal_ui_mpc <- federal_ui_mpc[1:17,1]
state_ui_mpc <- state_ui_mpc[1:17,1]
federal_subsidies_mpc <- federal_subsidies_mpc[1:17,1]
federal_aid_to_small_businesses_arp_mpc <- federal_aid_to_small_businesses_arp_mpc[1:17,1]
federal_other_direct_aid_arp_mpc <- federal_other_direct_aid_arp_mpc[1:17,1]
federal_other_vulnerable_arp_mpc <- federal_other_vulnerable_arp_mpc[1:17,1]
federal_student_loans_mpc <- federal_student_loans_mpc[1:17,1]
state_subsidies_mpc <- state_subsidies_mpc[1:17,1]
federal_health_outlays_mpc <- federal_health_outlays_mpc[1:17,1]
state_health_outlays_mpc <- state_health_outlays_mpc[1:17,1]

# Combine columns to form data frame 
mpc_data <- data.frame(
  federal_non_corporate_taxes_mpc, 
  state_non_corporate_taxes_mpc, 
  federal_corporate_taxes_mpc, 
  state_corporate_taxes_mpc,
  federal_social_benefits_mpc, 
  state_social_benefits_mpc, 
  rebate_checks_mpc, 
  rebate_checks_arp_mpc, 
  federal_ui_mpc, 
  state_ui_mpc, 
  federal_subsidies_mpc, 
  federal_aid_to_small_businesses_arp_mpc, 
  federal_other_direct_aid_arp_mpc, 
  federal_other_vulnerable_arp_mpc, 
  federal_student_loans_mpc, 
  state_subsidies_mpc, 
  federal_health_outlays_mpc, 
  state_health_outlays_mpc
) %>% 
  pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>%
  group_by(type) %>%
  mutate(index = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = index, values_from = value) %>% 
  rename_with(~ paste0("Q", .)) %>% 
  rename(Variable = Qtype)

# Export the data frame to an excel sheet
openxlsx::write.xlsx(mpc_data, 'shiny/cache/mpcs.xlsx', overwrite = TRUE)