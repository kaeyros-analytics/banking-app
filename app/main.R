#C:/Kaeyros/compte dormant/bank/app/main.R

box::use(app/logic/data_transformation,)
box::use(app/view/date_range_module,
         app/view/gender_filter_module,
         app/view/city_filter_module,
         app/view/status_filter_module,
         app/view/action_button_module,
         )
box::use(shiny[h3, moduleServer, NS],shinydashboard[dashboardPage,dashboardSidebar],htmltools)


#' @export
ui <- dashboardPage(
  ns <- NS(id),
  dashboardHeader(
    title = h1("Dormant Analysis", style = "font-size: 25px; font-weight: bold; color: #00ff00;") 
  ),
  dashboardSidebar(
    date_range_module$ui(ns("dateRangeInput")),
    gender_filter_module$ui(ns("genderFilter")),
    status_filter_module$ui(ns("statusFilter")),
    city_filter_module$ui(ns("cityFilter")),
    action_button_module$ui(ns("loadDataButton"))
  ),
  dashboardBody(
    total_customers_module$ui(ns("totalCustomersModule"))
  )
)


server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      list(account = data_transformation$account, transactions = data_transformation$transactions)
    })
    
    callModule(date_range_module, "dateRangeInput",data)
    callModule(total_customers_module, "totalCustomersModule", data)
    
    total_customers_df <- callModule(total_customers_module, "totalCustomersModule", data)
    
    output$message <- renderUI({
      div(
        style = "display: flex; justify-content: center; align-items: center; height: 100vh;",
        tags$h1(
          tags$a("Check out Rhino docs!", href = "https://appsilon.github.io/rhino/")
        )
      )
    })
  })
}
