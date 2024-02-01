# modules/city_filter_module.R
box::use(shiny[h3, moduleServer, NS, dateRangeInput],)

#' Module to create a selectInput for city filtering
#' @export
city_filter_module <- function(id, city_choices) {
  ns <- NS(id)
  
  selectInput(
    ns("cityFilter"),
    "Select City:",
    city_choices,
    selected = "All"
  )
}

#' @export
cityFilterModule <- function(input, output, session, city_choices) {
  return(NULL)
}
