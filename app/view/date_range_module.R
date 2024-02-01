# C:/Kaeyros/compte dormant/bank/app/view/date_range_module.R

box::use(shiny[h3, moduleServer, NS, dateRangeInput],)


#' Module to create a dateRangeInput
#' @export
date_range_module <- function(id) {
  ns <- NS(id)
  
  dateRangeInput(
    ns("dateRangeInput"),
    label = "Period",
    startview = "year",
    start = min(account$creation_date),
    end = max(transactions$transaction_date),
    min = min(account$creation_date),
    max = max(transactions$transaction_date),
    language = "en",
    separator = " - ",
    width = "100%",
    weekstart = 1
  )
}