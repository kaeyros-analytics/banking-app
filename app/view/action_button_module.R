# modules/action_button_module.R

box::use(shiny[h3, moduleServer, NS, actionButton],)

#' Module to create an actionButton
#' @export
action_button_module <- function(id, label) {
  ns <- NS(id)
  actionButton(
    ns("loadDataButton"),
    label
  )
}

#' @export
actionButtonModule <- function(input, output, session) {
  return(NULL)
}
