#C:/Kaeyros/compte dormant/bank/app/view/gender_filter_module.R
box::use(shiny[h3, moduleServer, NS, selectInput],)

#' Module to create a gender filter selectInput
#' @export
gender_filter_module <- function(id) {
  ns <- NS(id)
  
  selectInput(ns("genderFilter"), "Select Gender:", c("Male", "Female", "All"), selected = "All")
}

#' @export
genderFilterModule <- function(input, output, session) {
  return(NULL)
}
