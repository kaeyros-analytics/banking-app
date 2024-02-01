#C:/Kaeyros/compte dormant/bank/app/view/total_customers_module.R
box::use(shiny[h3, moduleServer, NS, dateRangeInput],)

#' total_customers_module
#' @export
total_customers_module <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    totalCustomers_df <- reactive({
      if (input$genderFilter == "All") {
        data$account|>
          left_join(data$transactions, by = "account_id", relationship = "many-to-many")|> 
          filter(
            transaction_date >= input$dateRangeInput[1],
            transaction_date <= input$dateRangeInput[2],
            status == input$statusFilter,
            if (input$cityFilter != "All") transaction_city == input$cityFilter else TRUE
          )
      } else {
        data$account|>
          left_join(data$transactions, by = "account_id", relationship = "many-to-many")|> 
          filter(
            transaction_date >= input$dateRangeInput[1],
            transaction_date <= input$dateRangeInput[2],
            gender == input$genderFilter,
            status == input$statusFilter,
            if (input$cityFilter != "All") transaction_city == input$cityFilter else TRUE
          )
      }
    })
    
    return(reactive({
      totalCustomers_df()
    }))
  })
}
