dashboardPage(
  dashboardHeader(
    title = tags$h3(tags$b("Dormant Analysis"))
  ),
  dashboardSidebar(
    conditionalPanel(
      condition = "input.tab == 'General' || input.tab == 'Inactive' || input.tab == 'Localisation'",
      dateRangeInput("dateRangeInput",
                     label = "Period",
                     startview = "year",
                     start = min(account$creation_date),
                     end = max(transactions$transaction_date),
                     min = min(account$creation_date),
                     max = max(transactions$transaction_date),
                     language = "en", separator = " - ", width = "100%", weekstart = 1),
      selectInput("genderFilter", "Select Gender:", c("Men", "Women", "All"), selected = "All"),
      #selectizeInput("statusFilter", "Select Status:", choices = c("Inactive", "Active"), multiple = TRUE, selected = c("Inactive", "Active")),
      selectInput("statusFilter", "Select Status:", c("Inactive", "Active", "All"), selected = "All"),
      selectInput("cityFilter", "Select City:", c(unique(account$location), "All"), selected = "All"),
      actionButton("loadDataButton", "Apply filters")
    ),
    conditionalPanel(
      condition = "input.tab == 'Predictive'",
      selectInput("client_id", "Client_ID:", c(unique(predict_data$Client_id)))
    )
  ),
  dashboardBody(
    tabsetPanel(
      id = "tab",
      tabPanel("General",
               fluidRow(
                 valueBoxOutput("totalCustomers", width = 3),
                 valueBoxOutput("activeCustomers", width = 3),
                 valueBoxOutput("inactiveCustomers", width = 3),
                 valueBoxOutput("revenueLoss", width = 3)
               ),
               fluidRow(
                 box(width = 6, shinycssloaders::withSpinner(plotlyOutput("totalCustomersByGender"), type = 5, color = 'blue')),
                 box(width = 6, shinycssloaders::withSpinner(plotlyOutput("totalCustomersByMarital"), type = 5, color = 'blue'))
               ),
               fluidRow(
                 box(width = 6, shinycssloaders::withSpinner(plotlyOutput("CustomersStatusByAgency"), type = 1, color = 'blue')),
                 box(width = 6, shinycssloaders::withSpinner(plotlyOutput("CustomersStatusByActivity"), type = 1, color = 'blue'))
               )
      ),
      tabPanel("Inactive",
               fluidRow(
                 box(width = 6, shinycssloaders::withSpinner(plotlyOutput("CustomersStatusByBalanceClass"), type = 5, color = 'blue')),
                 box(width = 6, shinycssloaders::withSpinner(plotlyOutput("CustomersStatusByAgeClass"), type = 5, color = 'blue'))
               ),
               fluidRow(
                 box(width = 12, shinycssloaders::withSpinner(plotlyOutput("TemporelSerie"), type = 1, color = 'blue'))
               )
      ),
      tabPanel("Localisation",
               fluidRow(
                 box(width = 12, shinycssloaders::withSpinner(leafletOutput("Carte"), type = 5, color = 'blue'))
               ),
               fluidRow(
                 downloadButton("downloadCSV", tags$b("Download Table")),
                 box(width = 12, tags$h3(tags$b("List of Inactive Accounts")), shinycssloaders::withSpinner(rHandsontableOutput('Table'), type = 1, color = 'blue'))
               )
      ),
      tabPanel("Predictive",
               fluidRow(
                 box(width = 12, plotlyOutput("DefaultEvolution"))
               ),
               fluidRow(
                 downloadButton("downloadCSV1", tags$b("Download Table")),
                 box(width = 12, tags$h3(tags$b("Inactive probability table over six months")), formattableOutput('Table1'))
               )
      )
    )
  )
)
