# app.R



# Load your dataset (replace 'your_dataset.csv' with your actual dataset file)

ui <- fluidPage(
  tags$style(HTML("
    .sidebar {
      width: 100px; 
    }
  ")),
  sidebarLayout(
    sidebarPanel(
      # Sidebar Input
      h1(
        HTML("<span style='font-size: 50px; color: #3498db; text-shadow: 2px 2px 4px #666;'>Dormant Analysis</span>")
      ),
      dateRangeInput("dateRangeInput",
                     label = "Period", 
                     startview = "year",
                     start = min(account$creation_date),
                     end = max(transactions$transaction_date),
                     min = min(account$creation_date),
                     max = max(transactions$transaction_date),
                     language = "en", separator = " - ", width = "100%", weekstart = 1),
      selectInput("genderFilter", "Select Gender:", c("Homme","Femme","All"), selected = "All"),
      selectizeInput("statusFilter", "Select Status:", choices = c("Active", "Inactive"), multiple = TRUE, selected = c("Active", "Inactive")),
      selectInput("cityFilter", "Select City:", c(unique(transactions$transaction_city),"All"), selected = "All"),
      actionButton("loadDataButton", "Apply filters"),
    ),
    mainPanel(
      # Main Panel Metrics
      fluidRow(
        column(width = 4,valueBoxOutput("totalCustomers")),
        column(width = 4, valueBoxOutput("activeCustomers")),
        column(width = 4,valueBoxOutput("inactiveCustomers"))
      ),
      
      # Additional Analysis
      fluidRow(
        #column(width = 6,plotOutput("totalCustomersByMarital")),
        column(width = 6,highchartOutput("totalCustomersByMarital")),
        column(width = 6,highchartOutput("totalCustomersByGender"))
      ),
      
      fluidRow(
        column(width = 6,highchartOutput("transactionsByType")),
        column(width = 6,highchartOutput("transactionsByGender"))
        )
      )
  )
)

server <- function(input, output) {
  observeEvent(input$loadDataButton, {
    # Filtered data based on user inputs
    totalCustomers_df <- reactive({
      
      if(input$genderFilter == "All"){
         account %>%
          left_join(transactions, by = "account_id",relationship = "many-to-many") %>% 
          filter(transaction_date >= input$dateRangeInput[1],
                 transaction_date <= input$dateRangeInput[2],
                 #if () gender == input$genderFilter else TRUE,
                 status == input$statusFilter,
                 if (input$cityFilter != "All") transaction_city == input$cityFilter else TRUE
                 #transaction_city == input$cityFilter
                 )
      } else{
        account %>%
          left_join(transactions, by = "account_id",relationship = "many-to-many") %>% 
          filter(transaction_date >= input$dateRangeInput[1],
                 transaction_date <= input$dateRangeInput[2],
                 gender == input$genderFilter,
                 status == input$statusFilter,
                 if (input$cityFilter != "All") transaction_city == input$cityFilter else TRUE
                 #transaction_city == input$cityFilter
                 )
      }
      
    })
    
    
    
    #Main Panel Metrics
    output$totalCustomers <- renderInfoBox({
      customers <- length(unique(totalCustomers_df()$account_id))

      tags$div(
        style = "width: 100%; height: 100%; margin: auto;", 
        infoBox(
          "Total Customers",
          value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;",customers),  
          icon = icon("users", class = "fa-4x"),  
          color = "olive",
          width = 12  
        )
      )
    })
    
    output$activeCustomers <- renderInfoBox({
      df_active <- totalCustomers_df() %>%
        filter(status== "Active")
      
      tags$div(
        style = "width: 100%; height: 100%; margin: auto;", 
        infoBox(
          "Active Customers",
          value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;", 
                         length(unique(df_active$account_id))),
          icon = icon("users", class = "fa-4x"),  
          color = "olive",
          width = 12  
        )
      )
    })
    
    output$inactiveCustomers <- renderInfoBox({
      df_inactive <- totalCustomers_df() %>%
        filter(status== "Inactive")
      
      tags$div(
        style = "width: 100%; height: 100%; margin: auto;", 
        infoBox(
          "Inactive Customers",
          value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;", 
                         length(unique(df_inactive$account_id))), 
          icon = icon("users", class = "fa-4x"),  
          color = "teal",
          width = 12  
        )
      )
    })
    
    # Additional Analysis
    output$totalCustomersByMarital <- renderHighchart({
      # Données
      df <- totalCustomers_df() %>%
        select(account_id, marital_status) %>%
        distinct(account_id, marital_status, .keep_all = TRUE) %>%
        group_by(marital_status) %>%
        summarise(total = n())
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = df$marital_status) %>%
        hc_yAxis(title = list(text = "Total Customers")) %>%
        hc_add_series(name = "Customers", data = df$total) %>%
        hc_title(text = "Customers by Marital Status") %>%
        hc_plotOptions(series = list(stacking = "normal"))  
    })
    
    output$totalCustomersByGender <- renderHighchart({
      # Données
      df1 <- totalCustomers_df() %>%
        select(account_id, gender) %>%
        distinct(account_id, gender, .keep_all = TRUE) %>%
        group_by(gender) %>%
        summarise(total = n())
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = df1$gender) %>%
        hc_add_series(name = "Customers",data = df1$total) %>%
        hc_title(text = "Customers by Marital Status") %>%
        hc_plotOptions(series = list(stacking = "normal"))  
    })
    
    output$transactionsByType <- renderHighchart({
      # Données
      df1 <- totalCustomers_df() %>%
        select(transaction_id,transaction_type) %>%
        distinct(transaction_id,transaction_type, .keep_all = TRUE) %>%
        group_by(transaction_type) %>%
        summarise(total = n())
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = df1$transaction_type) %>%
        hc_add_series(name = "Transactions",data = df1$total) %>%
        hc_title(text = "Numbers transactions by type") %>%
        hc_plotOptions(series = list(stacking = "normal"))  
    })
    
    output$transactionsByGender <- renderHighchart({
      # Données
      df1 <- totalCustomers_df() %>%
        select(transaction_id,gender) %>%
        distinct(transaction_id,gender, .keep_all = TRUE) %>%
        group_by(gender) %>%
        summarise(total = n())
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = df1$gender) %>%
        hc_add_series(name = "Transactions",data = df1$total) %>%
        hc_title(text = "Numbers transactions by gender") %>%
        hc_plotOptions(series = list(stacking = "normal"))  
    })
    
  })
}
shinyApp(ui, server)
