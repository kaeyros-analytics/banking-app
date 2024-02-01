box::use(shiny[h3, moduleServer, NS, selectizeInput],)


#' Module to create a selectizeInput for status filtering
#' @export
status_filter_module <- function(id) {
  ns <- NS(id)
  
  selectizeInput(
    ns("statusFilter"),
    "Select Status:",
    choices = c("Active", "Inactive"),
    multiple = TRUE,
    selected = c("Active", "Inactive")
  )
}

#' @export
statusFilterModule <- function(input, output, session) {
  return(NULL)
}
