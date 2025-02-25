#' Title
#'
#' @param .data 
#' @param variable 
#'
#' @return
#' @export
#'
#' @examples
comparison_plot <- function(.data, variable){
  
  
  plot <- .data %>% 
    filter(variable == {{ variable }}) %>% 
    ggplot(aes(x = date,  y =  value, fill = source)) +
    #geom_col(position=position_dodge2(reverse = TRUE)) +
    geom_col(position=position_dodge2(reverse = TRUE)) +
    labs(title = glue::glue("{snakecase::to_title_case(variable)}"),
         x = NULL,
         y = NULL) +
    ggbrookings::theme_brookings() +
    ggbrookings::scale_fill_brookings(
                      name = "",
                      labels = c('Current', 'Previous')) +
    scale_x_yearquarter(breaks = waiver(),
                        date_breaks = '3 months',
                        date_labels = "Q%q") +
    facet_grid( ~ year(date),
                space = "free_x",
                scales = "free_x",
                switch = "x")  +
    theme(legend.position = 'top') +
    guides(fill = guide_legend(reverse = TRUE)) 
  
  
  variable_name <- rlang::as_name(rlang::ensym(variable))
  
  if(str_ends(variable_name, 'contribution')){
    plot + 
      scale_y_continuous(name = '', 
                         labels = scales::label_percent(scale = 1))
  } else {
    plot +
      scale_y_continuous(name = '', 
                         labels = scales::label_comma())
  }
  
  if (variable_name %in% c(
    'federal_purchases',
    'federal_purchases_contribution',
    'state_purchases',
    'state_purchases_contribution')) {
    plot + labs(subtitle = 'NIPA Consistent')
  }
    else if(variable_name %in% c('federal_contribution', 'state"contribution')) {
        plot + labs(subtitle = 'FIM Consistent')
      }
  else if
    (variable_name %in% c(
      'federal_social_benefits',
      'federal_social_benefits_contribution',
      'state_social_benefits',
      'state_social_benefits_contribution'
    )) {
      plot + labs(subtitle = 'Social Benefits Remainder')
    }
  else{
    plot
  }
  
}
