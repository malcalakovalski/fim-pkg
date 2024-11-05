##########################
# Update Comparison File #
##########################

# This script generates an html file called update-comparison that is stored in the results folder. 
# This file -- colloquially called The Manu File -- allows us to compare our FIM results to what we published in the previous month. 
# You must run fiscal_impact_BETA prior to running this script. It draws in an Excel file containing
# the FIM contributions. It generates tables and figures comparing this month's results to last month's results. 

# Set Current Quarter
current_quarter <- yearquarter(Sys.Date()) - 1

#------------------- Calculate Real Levels ----------------------------#

# Load in previous month's inputs
previous_inputs <- 
  readxl::read_xlsx(glue('results/{last_month_year}/beta/inputs-{last_month_year}.xlsx')) %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2020 Q1" ~ as.character(current_quarter + 8)) %>% 
  select(-id, 
         -recession)

# Load the current month's inputs 
current_inputs <- 
  readxl::read_xlsx(glue('results/{month_year}/beta/inputs-{month_year}.xlsx')) %>%
  drop_na(date) %>%
  mutate(date = yearquarter(date)) %>%  
  as_tsibble(index = date) %>%
  filter_index("2020 Q1" ~ as.character(current_quarter + 8)) %>%
  select(-id, 
         -recession)

# Use the nominal values and conusmption deflator to calculate the real levels 
# Previous
previous_inputs <- previous_inputs %>%
  mutate(
    federal_ui_real = federal_ui - lag(federal_ui) * consumption_deflator_growth,
    state_ui_real = state_ui - lag(state_ui) * consumption_deflator_growth,
    federal_subsidies_real = federal_subsidies - lag(federal_subsidies) * consumption_deflator_growth,
    state_subsidies_real = state_subsidies - lag(state_subsidies) * consumption_deflator_growth,
    federal_health_outlays_real = federal_health_outlays - lag(federal_health_outlays) * consumption_deflator_growth,
    state_health_outlays_real = state_health_outlays - lag(state_health_outlays) * consumption_deflator_growth,
    federal_social_benefits_real = federal_social_benefits - lag(federal_social_benefits) * consumption_deflator_growth,
    state_social_benefits_real = state_social_benefits - lag(state_social_benefits) * consumption_deflator_growth,
    federal_corporate_taxes_real = federal_corporate_taxes - lag(federal_corporate_taxes) * consumption_deflator_growth,
    state_corporate_taxes_real = state_corporate_taxes - lag(state_corporate_taxes) * consumption_deflator_growth,
    federal_non_corporate_taxes_real = federal_non_corporate_taxes - lag(federal_non_corporate_taxes) * consumption_deflator_growth,
    state_non_corporate_taxes_real = state_non_corporate_taxes - lag(state_non_corporate_taxes) * consumption_deflator_growth,
    rebate_checks_arp_real = rebate_checks_arp - lag(rebate_checks_arp) * consumption_deflator_growth,
    federal_other_direct_aid_arp_real = federal_other_direct_aid_arp - lag(federal_other_direct_aid_arp) * consumption_deflator_growth,
    federal_other_vulnerable_arp_real = federal_other_vulnerable_arp - lag(federal_other_vulnerable_arp) * consumption_deflator_growth,
    federal_aid_to_small_businesses_arp_real = federal_aid_to_small_businesses_arp - lag(federal_aid_to_small_businesses_arp) * consumption_deflator_growth,
    federal_student_loans_real = federal_student_loans - lag(federal_student_loans) * consumption_deflator_growth,
    supply_side_ira_real = supply_side_ira - lag(supply_side_ira) * consumption_deflator_growth,
    rebate_checks_real = rebate_checks - lag(rebate_checks) * consumption_deflator_growth,
    federal_purchases_real = federal_purchases - lag(federal_purchases) * consumption_deflator_growth,
    state_purchases_real = state_purchases - lag(state_purchases) * consumption_deflator_growth,
    consumption_grants_real = consumption_grants - lag(consumption_grants) * consumption_deflator_growth,
    investment_grants_real = investment_grants - lag(investment_grants) * consumption_deflator_growth
  )
# Current 

current_inputs <- current_inputs %>%
  mutate(
    federal_ui_real = federal_ui - lag(federal_ui) * consumption_deflator_growth,
    state_ui_real = state_ui - lag(state_ui) * consumption_deflator_growth,
    federal_subsidies_real = federal_subsidies - lag(federal_subsidies) * consumption_deflator_growth,
    state_subsidies_real = state_subsidies - lag(state_subsidies) * consumption_deflator_growth,
    federal_health_outlays_real = federal_health_outlays - lag(federal_health_outlays) * consumption_deflator_growth,
    state_health_outlays_real = state_health_outlays - lag(state_health_outlays) * consumption_deflator_growth,
    federal_social_benefits_real = federal_social_benefits - lag(federal_social_benefits) * consumption_deflator_growth,
    state_social_benefits_real = state_social_benefits - lag(state_social_benefits) * consumption_deflator_growth,
    federal_corporate_taxes_real = federal_corporate_taxes - lag(federal_corporate_taxes) * consumption_deflator_growth,
    state_corporate_taxes_real = state_corporate_taxes - lag(state_corporate_taxes) * consumption_deflator_growth,
    federal_non_corporate_taxes_real = federal_non_corporate_taxes - lag(federal_non_corporate_taxes) * consumption_deflator_growth,
    state_non_corporate_taxes_real = state_non_corporate_taxes - lag(state_non_corporate_taxes) * consumption_deflator_growth,
    rebate_checks_arp_real = rebate_checks_arp - lag(rebate_checks_arp) * consumption_deflator_growth,
    federal_other_direct_aid_arp_real = federal_other_direct_aid_arp - lag(federal_other_direct_aid_arp) * consumption_deflator_growth,
    federal_other_vulnerable_arp_real = federal_other_vulnerable_arp - lag(federal_other_vulnerable_arp) * consumption_deflator_growth,
    federal_aid_to_small_businesses_arp_real = federal_aid_to_small_businesses_arp - lag(federal_aid_to_small_businesses_arp) * consumption_deflator_growth,
    federal_student_loans_real = federal_student_loans - lag(federal_student_loans) * consumption_deflator_growth,
    supply_side_ira_real = supply_side_ira - lag(supply_side_ira) * consumption_deflator_growth,
    rebate_checks_real = rebate_checks - lag(rebate_checks) * consumption_deflator_growth,
    federal_purchases_real = federal_purchases - lag(federal_purchases) * consumption_deflator_growth,
    state_purchases_real = state_purchases - lag(state_purchases) * consumption_deflator_growth,
    consumption_grants_real = consumption_grants - lag(consumption_grants) * consumption_deflator_growth,
    investment_grants_real = investment_grants - lag(investment_grants) * consumption_deflator_growth
  )

#------------------- Contributions Figures ----------------------------#

# This section of the code pulls in the contributions and re-formats the data. 

# Load previous month's results
previous <-
  readxl::read_xlsx(glue('results/{last_month_year}/beta/contributions-{last_month_year}.xlsx')) %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2020 Q1" ~ as.character(current_quarter + 8)) %>% 
  select(-id, 
         -recession)

# Load in current month's results 
current <- 
  readxl::read_xlsx(glue('results/{month_year}/beta/contributions-{month_year}.xlsx')) %>%
  drop_na(date) %>%
  mutate(date = yearquarter(date)) %>%  
  as_tsibble(index = date) %>%
  filter_index("2020 Q1" ~ as.character(current_quarter + 8)) %>%
  select(-id, 
         -recession)

#-------------- Clean the Data -------------------------------------# 

# Reshape and join the data 
previous_long <- pivot_longer(previous, cols = where(is.numeric), values_to = 'previous')
current_long <- pivot_longer(current, cols = where(is.numeric), values_to = 'current')
previous_inputs_long <- pivot_longer(previous_inputs, cols = where(is.numeric), values_to = 'previous')
current_inputs_long <- pivot_longer(current_inputs, cols = where(is.numeric), values_to = 'current')

# Join Contributions 
contributions_comparison <- inner_join(current_long,
                         previous_long,
                         by = c('date', 'name')) %>% 
  rename(variable = name) %>% 
  as_tsibble(index = date) 


inputs_comparison <- inner_join(current_inputs_long,
                                previous_inputs_long,
                                by = c('date', 'name')) %>% 
  rename(variable = name) %>% 
  as_tsibble(index = date) 

# Append 
comparison <- bind_rows(inputs_comparison, contributions_comparison)

comparison_long <-
  comparison %>%
  pivot_longer(c(previous, current),
               names_to = 'source')

# Define the "components", i.e. the data we want to include in our contributions comparison plots 
components <- c(
  "federal_purchases_contribution",
  "federal_purchases", 
  "federal_purchases_real", 
  "consumption_grants_contribution", 
  "consumption_grants",
  "consumption_grants_real",
  "investment_grants_contribution",
  "investment_grants",
  "investment_grants_real",
  "state_purchases_contribution", 
  "state_purchases",
  "state_purchases_real",
  "federal_non_corporate_taxes_contribution",
  "federal_non_corporate_taxes",
  "federal_non_corporate_taxes_real",
  "state_non_corporate_taxes_contribution", 
  "state_non_corporate_taxes",
  "state_non_corporate_taxes_real",
  "federal_corporate_taxes_contribution", 
  "federal_corporate_taxes",
  "federal_corporate_taxes_real",
  "supply_side_ira_contribution", 
  "supply_side_ira",
  "supply_side_ira_real",
  "state_corporate_taxes_contribution", 
  "state_corporate_taxes",
  "state_corporate_taxes_real",
  "federal_social_benefits_contribution", 
  "federal_social_benefits",
  "federal_social_benefits_real",
  "state_social_benefits_contribution", 
  "state_social_benefits",
  "state_social_benefits_real",
  "rebate_checks_contribution", 
  "rebate_checks",
  "rebate_checks_real",
  "rebate_checks_arp_contribution", 
  "rebate_checks_arp",
  "rebate_checks_arp_real",
  "federal_ui_contribution", 
  "federal_ui",
  "federal_ui_real",
  "state_ui_contribution", 
  "state_ui",
  "state_ui_real",
  "federal_subsidies_contribution",  
  "federal_subsidies",
  "federal_subsidies_real", 
  "federal_aid_to_small_businesses_arp_contribution", 
  "federal_aid_to_small_businesses_arp",
  "federal_aid_to_small_businesses_arp_real", 
  "federal_other_direct_aid_arp_contribution", 
  "federal_other_direct_aid_arp",
  "federal_other_direct_aid_arp_real", 
  "federal_other_vulnerable_arp_contribution", 
  "federal_other_vulnerable_arp",
  "federal_other_vulnerable_arp_real", 
  "federal_student_loans_contribution",
  "federal_student_loans",
  "federal_student_loans_real",
  "state_subsidies_contribution", 
  "state_subsidies",
  "state_subsidies_real", 
  "federal_health_outlays_contribution", 
  "federal_health_outlays",
  "federal_health_outlays_real", 
  "state_health_outlays_contribution", 
  "state_health_outlays",
  "state_health_outlays_real", 
  "federal_contribution",
  "federal",
  "federal_real",
  "state_contribution", 
  "state",
  "state_real", 
  "taxes_contribution", 
  "taxes",
  "taxes_real",
  "transfers_contribution",
  "transfers",
  "transfers_real",
  "fiscal_impact_measure"
)

# Define the comparison_ga function, which pulls in data (federal purchases contribution, for example) and generates a plot comparing the previous month's result to the current month's result
comparison_ga <- function(.data, variable){
  plot <- .data %>% 
    filter(variable == {{ variable }}) %>% 
    ggplot(aes(x = date,  y =  value, fill = source)) +
    #geom_col(position=position_dodge2(reverse = TRUE)) +
    geom_col(position=position_dodge2(reverse = TRUE)) +
    labs(title = glue::glue("{snakecase::to_title_case(variable)}"),
         x = NULL,
         y = NULL,
         fill = NULL) +
    scale_x_yearquarter(breaks = waiver(),
                        date_breaks = '3 months',
                        date_labels = "Q%q") +
    facet_grid( ~ year(date),
                space = "free_x",
                scales = "free_x",
                switch = "x")  +
    theme(legend.position = 'top', 
          plot.title = element_text(face = "bold", size = 16, 
                                    family = "sans", 
                                    color = "gray20"),
          axis.text = element_text(size = 12, 
                                   family = "sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = c('current'="royalblue4", 
                                 'previous' = "darkgray"),
                      labels = c('Current', 'Previous')) 
}


comparison_nested <-
  comparison_long %>%
  filter(variable %in% components) %>% 
  group_by(variable) %>%
  nest() %>%
  mutate(plot = map2(.x = variable,
                     .y = data,
                     .f = ~comparison_ga(.data = .y,
                                         variable = .x)))



write_rds(comparison_nested, 'data/comparison_nested')
plots <- rlang::set_names(comparison_nested$plot, comparison_nested$variable)
write_rds(plots, 'data/plots')


# Get Table-----------------------------------------

current_summary <- 
  current %>%
  select(date, federal_contribution, state_contribution, taxes_contribution, transfers_contribution, fiscal_impact_measure) %>% 
  pivot_longer(
    values_to = 'Current',
    names_to = "name", 
    -date
  ) 

previous_summary <-
  previous %>% 
  select(date, federal_contribution, state_contribution, taxes_contribution, transfers_contribution, fiscal_impact_measure) %>%
  pivot_longer(
    values_to = 'Previous',
    names_to = "name",
    -date
  )

summary <- inner_join(current_summary,
                      previous_summary,
                      by = c("date", "name")) %>% 
  mutate(Difference = Current - Previous) %>%
  mutate(name= case_when(
    name == "state_contribution" ~ "State Purchases",
    name == "federal_contribution" ~ "Federal Purchases",
    name == "fiscal_impact_measure" ~ "Fiscal Impact Measure",
    name == "taxes_contribution" ~ "Taxes",
    name == "transfers_contribution" ~ "Transfers",
    TRUE ~ "Other"
  )) %>%
  mutate(name = factor(name, levels = c("Fiscal Impact Measure", "Federal Purchases", "State Purchases", "Taxes", "Transfers"))) %>%
  arrange(date, name)


summary_tbl <- 
  summary %>% 
  as_tibble() %>%
  group_by(date) %>% 
  mutate(date = as.character(date)) %>% 
  gt(groupname_col = 'date') %>% 
  tab_style(locations = cells_title(groups = "title"),
            style = list(
              cell_text(weight = "bold", size = 24)
            )
  ) %>% 
  
  
  opt_row_striping() %>%
  tab_header(title = md('FIM Components Summary')) %>%
  fmt_percent(where(is.numeric),
              scale_values = FALSE) %>% 
  
  opt_all_caps() %>%
  opt_table_font(
    font = list(
      google_font("Roboto"),
      default_fonts()))  %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)),
      #Make text bold
      cell_text(weight = "bold"))) %>%
  tab_options(
    column_labels.border.top.width = px(10),
    # column_labels.border.top.color = "transparent",
    # table.border.top.color = "transparent",
    # table.border.bottom.color = "transparent",
    heading.background.color = 'royalblue4',
    data_row.padding = px(10),
    source_notes.font.size = 14,
    heading.align = "center",
    row_group.background.color = "#D0D3D4") %>% 
  tab_style(  style = list(
    cell_text(weight = "bold")
  ),
  locations = list(cells_body(rows = name == 'Fiscal Impact Measure')))








