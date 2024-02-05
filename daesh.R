ui <- dashboardPage(
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
      selectizeInput("statusFilter", "Select Status:", choices = c("Inactive", "Active"), multiple = TRUE, selected = c("Inactive", "Active")),
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
                 box(width = 6, shinycssloaders::withSpinner(highchartOutput("totalCustomersByGender"), type = 5, color = 'blue')),
                 box(width = 6, shinycssloaders::withSpinner(highchartOutput("totalCustomersByMarital"), type = 5, color = 'blue'))
               ),
               fluidRow(
                 box(width = 6, shinycssloaders::withSpinner(highchartOutput("CustomersStatusByAgency"), type = 1, color = 'blue')),
                 box(width = 6, shinycssloaders::withSpinner(highchartOutput("CustomersStatusByActivity"), type = 1, color = 'blue'))
               )
      ),
      tabPanel("Inactive",
               fluidRow(
                 box(width = 6, shinycssloaders::withSpinner(highchartOutput("CustomersStatusByBalanceClass"), type = 5, color = 'blue')),
                 box(width = 6, shinycssloaders::withSpinner(highchartOutput("CustomersStatusByAgeClass"), type = 5, color = 'blue'))
               ),
               fluidRow(
                 box(width = 12, shinycssloaders::withSpinner(highchartOutput("TemporelSerie"), type = 1, color = 'blue'))
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
                 box(width = 12, highchartOutput("DefaultEvolution"))
               ),
               fluidRow(
                 downloadButton("downloadCSV1", tags$b("Download Table")),
                 box(width = 12, tags$h3(tags$b("Customer default probability table over six months")), formattableOutput('Table1'))
               )
      )
    )
  )
)


server <- function(input, output) {
  observeEvent(input$loadDataButton, {
    # Filtered data based on user inputs
    
    # Filtrer les comptes en fonction du genre sélectionné dans input$genderFilter
    account_filter_by_gender <- account %>% 
      filter(if (input$genderFilter != "All") gender == input$genderFilter else TRUE) %>% 
      filter(if (input$cityFilter != "All") location == input$cityFilter else TRUE)
    
    # Filtrer les transactions en fonction de la date maximum dans input$dateRangeInput
    # et éventuellement en fonction de la ville sélectionnée dans input$cityFilter
    transactions_filter_by_date2 <- transactions %>% 
      filter(transaction_date <= input$dateRangeInput[2])
    
    
    # Jointure interne des comptes et des transactions filtrées par la date
    inner_account_transactions <- account_filter_by_gender %>% 
      inner_join(transactions_filter_by_date2, by = "account_id",relationship = "many-to-many") %>% 
      group_by(account_id) %>%
      # Calcul de la date de la dernière transaction pour chaque compte
      mutate(last_transaction_date = max(transaction_date, na.rm = TRUE))
    
    # Filtrer les transactions pour inclure uniquement celles
    # qui se trouvent dans la plage de dates spécifiée dans input$dateRangeInput
    # et ajouter une colonne de statut en fonction de la différence entre la dernière transaction et la date maximale de la plage
    totalCustomers_df <- inner_account_transactions %>% 
      filter(last_transaction_date >= input$dateRangeInput[1] & last_transaction_date <= input$dateRangeInput[2]) %>% 
      mutate(status = if_else((as.Date(input$dateRangeInput[2]) - last_transaction_date) > 60, "Inactive", "Active")) %>% 
      # Filtrer les résultats en fonction des statuts sélectionnés dans input$statusFilter
      filter(status %in% input$statusFilter)
    totalCustomers_agios_df <- totalCustomers_df %>%
      # Jointure interne avec les données d'agios filtrées
      inner_join(agios %>% filter(year_agios <= input$dateRangeInput[2]), by = "account_id",relationship = "many-to-many")
    
    
    temporale_df<- inner_account_transactions %>% 
      select(account_id,last_transaction_date,location) %>% 
      filter(if (input$cityFilter != "All") location == input$cityFilter else TRUE) %>% 
      distinct(account_id,last_transaction_date)
    
    # sequence des dates alant de dateRangeInput[1] à dateRangeInput[2]
    sequence_dates <- seq(input$dateRangeInput[1], input$dateRangeInput[2], by = "month")
    # Arrondir chaque date au dernier jour du mois
    sequence_dates_arrondies <- lapply(sequence_dates, function(date) {
      ceiling_date(date, unit = "month") - days(1)
    })
    
    dictionnaire <- list()  # Initialisation du dictionnaire
    locale <- Sys.setlocale("LC_TIME", "en_GB.UTF-8")
    for (date in sequence_dates_arrondies) {
      nomber_Inactive <- nrow(temporale_df %>% filter(date[[1]] - last_transaction_date > 60))
      key <- format(date[[1]], "%b.%Y",locale=locale)
      value <- c(nomber_Inactive, nrow(temporale_df) - nomber_Inactive)
      dictionnaire[[key]] <- value
    }
    
    # Convertir le dictionnaire en dataframe
    df <- as.data.frame(do.call(rbind, lapply(names(dictionnaire), function(key) {
      c(date = key, number_Inactive = dictionnaire[[key]][1], number_Active = dictionnaire[[key]][2])
    })))
    # Spécifier le type de données pour les colonnes
    df$number_Inactive <- as.integer(df$number_Inactive)
    df$number_Active <- as.integer(df$number_Active)
    
    #Main Panel Metrics
    output$totalCustomers <- renderInfoBox({
      customers <- length(unique(totalCustomers_df$account_id))
      
      tags$div( 
        infoBox(
          HTML("<strong>Total Customers</strong>"),
          value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;",customers),  
          icon = icon("users"),  
          color = "olive",
          width = 12  
        )
      )
    })
    
    output$activeCustomers <- renderInfoBox({
      df_active <- totalCustomers_df %>%
        filter(status== "Active")
      
      tags$div(
        infoBox(
          HTML("<strong>Active Customers</strong>"),
          value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;", 
                         length(unique(df_active$account_id))),
          icon = icon("users"),  
          color = "teal",
          width = 12  
        )
      )
    })
    
    output$inactiveCustomers <- renderInfoBox({
      df_inactive <- totalCustomers_df %>%
        filter(status== "Inactive")
      
      tags$div(
        infoBox(
          HTML("<strong>Inactive Customers</strong>"),
          value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;", 
                         length(unique(df_inactive$account_id))), 
          icon = icon("users"),  
          color = "teal",
          width = 12  
        )
      )
    })
    
    output$revenueLoss <- renderInfoBox({
      df_inactive <- totalCustomers_df %>%
        filter(status== "Inactive")
      
      Inactive_accounts <- totalCustomers_agios_df %>% 
        filter(status == "Inactive",balance_after_agios<0)
      
      # financial impact of inactivity
      tags$div(
        infoBox(
          HTML("<strong>Revenue Loss</strong>"),
          value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;", 
                         paste(-round(sum(Inactive_accounts$balance_after_agios)/1000000,2)),"M"), 
          icon = icon("money-check-alt"),  
          color = "olive",
          width = 12  
        )
      )
    })
    
    # Additional Analysis
    output$totalCustomersByGender <- renderHighchart({
      # Données
      df1 <- totalCustomers_df %>%
        select(account_id, gender) %>%
        distinct(account_id, gender, .keep_all = TRUE) %>%
        group_by(gender) %>%
        summarise(total = n())
      
      highchart() %>%
        hc_chart(type = "pie") %>%
        hc_add_series(name = "Customers", data = list_parse2(df1),innerSize = "50%") %>%
        hc_title(text = "Customers by Gender")
    })
    
    output$totalCustomersByMarital <- renderHighchart({
      customers_by_marital <- totalCustomers_df %>%
        select(account_id,marital_status, status) %>%
        distinct(account_id,marital_status, .keep_all = TRUE) %>%
        group_by(marital_status, status) %>%
        summarise(customer_count = n())
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = customers_by_marital$marital_status) %>%
        hc_add_series(
          name = "Active", 
          data = customers_by_marital %>%
            filter(status == "Active") %>%
            pull(customer_count)
        ) %>%
        hc_add_series(
          name = "Inactive", 
          data = customers_by_marital %>%
            filter(status == "Inactive") %>%
            pull(customer_count)
        ) %>%
        hc_title(text = "Customers by marital situation") %>%
        hc_yAxis(title = list(text = "Number of Customers")) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(grouping = FALSE) 
    })
    
    output$CustomersStatusByAgency <- renderHighchart({
      # Calculer le nombre de clients par statut et par agence
      df_status_by_agency <- totalCustomers_df %>%
        select(account_id,location, status) %>%
        distinct(account_id,location, .keep_all = TRUE) %>%
        group_by(location, status) %>%
        summarise(customer_count = n())
      
      # Créer le graphique Highchart
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = unique(df_status_by_agency$location)) %>%
        hc_add_series(
          name = "Active", 
          data = df_status_by_agency %>%
            filter(status == "Active") %>%
            pull(customer_count)
        ) %>%
        hc_add_series(
          name = "Inactive", 
          data = df_status_by_agency %>%
            filter(status == "Inactive") %>%
            pull(customer_count)
        ) %>%
        hc_title(text = "Customer Status by city") %>%
        hc_yAxis(title = list(text = "Number of Customers")) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(grouping = FALSE)
    })
    
    output$CustomersStatusByActivity <- renderHighchart({
      df_status_by_activity <- totalCustomers_df %>%
        select(account_id,occupation, status) %>%
        distinct(account_id,occupation, .keep_all = TRUE) %>%
        group_by(occupation, status) %>%
        summarise(customer_count = n())
      
      
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = df_status_by_activity$occupation) %>%
        hc_add_series(
          name = "Active", 
          data = df_status_by_activity %>%
            filter(status == "Active") %>%
            pull(customer_count)
        ) %>%
        hc_add_series(
          name = "Inactive", 
          data = df_status_by_activity %>%
            filter(status == "Inactive") %>%
            pull(customer_count)
        ) %>%
        hc_title(text = "Customer Status by occupation") %>%
        hc_yAxis(title = list(text = "Number of Customers")) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(grouping = FALSE)
    })
    
    output$CustomersStatusByBalanceClass <- renderHighchart({
      
      df2 <- totalCustomers_agios_df %>%
        # Grouper les données par account_id
        group_by(account_id) %>%
        # Résumé des données pour chaque account_id
        summarize(
          # Valeur maximale de year_agios pour chaque account_id
          max_year_agios = max(year_agios),
          # Solde après agios correspondant à l'année maximale
          balance_after_agios = balance_after_agios[which.max(year_agios)],
          # Statut associé à l'année maximale
          status = status[which.max(year_agios)]
        )
      
      quantiles <- quantile(df2$balance_after_agios, probs = c(0.25, 0.5, 0.75))
      # Spécifier les intervalles personnalisés
      intervals <- c(-Inf, 0, 2100000, 4700000, 7100000, Inf)
      
      # Discrétiser la variable balance_after_agios en utilisant les intervalles spécifiés
      df2 <- df2 %>%
        mutate(balance_interval = cut(balance_after_agios, 
                                      breaks = intervals, 
                                      labels = c("<0M", "[0M,2.1M[", "[2.1M,4.7M[", "[4.7M,7.1M[", ">7.1M")))
      # Calcul du nombre total de clients pour chaque 'balance_interval'
      data_summary <- df2 %>%
        group_by(balance_interval, status) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(balance_interval) %>%
        mutate(total_count = sum(count)) %>%
        ungroup() %>%
        mutate(percentage = round(count / total_count * 100,2))
      
      # Créer le graphique Highcharter
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "Status of customers by balance") %>%
        hc_xAxis(categories = unique(data_summary$balance_interval)) %>%
        hc_yAxis(title = list(text = "Nombre de clients")) %>%
        hc_plotOptions(grouping = FALSE) %>%
        hc_add_series(name = "Active", data = data_summary[data_summary$status == "Active",]$count) %>%
        hc_add_series(name = "Inactive", data = data_summary[data_summary$status == "Inactive",]$count) %>%
        hc_legend(enabled = TRUE)
      
    })
    
    output$CustomersStatusByAgeClass <- renderHighchart({
      
      data_summary <- totalCustomers_df %>%
        select(account_id,age_interval, status) %>%
        distinct(account_id,age_interval, .keep_all = TRUE) %>%
        group_by(age_interval, status) %>%
        summarise(count = n()) 
      
      
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "Status of customers by age") %>%
        hc_xAxis(categories = unique(data_summary$age_interval)) %>%
        hc_yAxis(title = list(text = "Nombre de clients")) %>%
        hc_plotOptions(grouping = FALSE) %>%
        hc_add_series(name = "Active", data = data_summary[data_summary$status == "Active",]$count) %>%
        hc_add_series(name = "Inactive", data = data_summary[data_summary$status == "Inactive",]$count) %>%
        hc_legend(enabled = TRUE)
      
    })
    
    output$TemporelSerie <- renderHighchart({
      
      # Créer une structure de base Highchart
      hc <- highchart() %>%
        hc_chart(type = "line") %>%
        hc_title(text = "Temporal evolution of Inactive accounts") %>%
        hc_xAxis(categories = as.character(df$date), labels = list(format = "{value:%b.%Y}")) %>%
        hc_yAxis(title = list(text = "Count")) %>%
        hc_legend(enabled = TRUE) %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE, valueDecimals = 0) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_credits(enabled = FALSE)
      
      # Ajouter les séries en fonction de l'input$statusFilter
      if ("Inactive" %in% input$statusFilter && "Active" %in% input$statusFilter) {
        hc <- hc %>% 
          hc_add_series(data = df$number_Inactive, name = "Number of Inactive", color = "black") %>%
          hc_add_series(data = df$number_Active, name = "Number Active", color = "blue")
      } else if ("Inactive" %in% input$statusFilter) {
        hc <- hc %>% 
          hc_add_series(data = df$number_Inactive, name = "Number of Inactive", color = "black")
      } else if ("Active" %in% input$statusFilter) {
        hc <- hc %>% 
          hc_add_series(data = df$number_Active, name = "Number Active", color = "blue")
      }
      
      return(hc)
      
    })
    
    output$Carte <- renderLeaflet({
      df <- totalCustomers_df %>% 
        distinct(account_id, latitude,longitude, marital_status, last_name, first_name, age,gender, occupation, status) %>% 
        filter(status == "Inactive")
      
      # Créer une carte Leaflet
      leaflet(df) %>%
        addTiles() %>%
        addMarkers(
          lng = ~longitude, # longitude
          lat = ~latitude, # latitude
          popup = ~paste(
            "<b>Name:</b> ", first_name, " ", last_name, "<br>",
            "<b>Age:</b> ", age, "<br>",
            "<b>Marital Status:</b> ", marital_status, "<br>",
            "<b>Occupation:</b> ", occupation
          ),
          clusterOptions = markerClusterOptions()  # Ajout du regroupement de marqueurs
        ) %>%
        addScaleBar() %>%     # Ajout de l'échelle
        addCircleMarkers(
          color = ~ifelse(gender == "Men", "blue", "red") 
        ) %>% 
        addLegend("topright",
                  colors = c("blue", "red"),
                  labels = c("Men", "Women"),
                  title = "Gender",
                  opacity = 1
        )
    })
    
    output$Table <- renderRHandsontable({
      
      # Exemple de données pour le tableau
      data=totalCustomers_agios_df %>% 
        distinct(account_id,first_name,last_name,age,gender,occupation,location,average_monthly_revenue,
                 number_of_children,last_transaction_date,transaction_type,year_agios,balance_after_agios,status) %>%
        arrange(account_id, year_agios) %>%
        group_by(account_id) %>%
        slice_tail(n = 1) %>% 
        rename("current_balance"= balance_after_agios) %>% 
        filter(status=="Inactive")
      
      colnames(data) <- c("account_id", "First Name", "Last Name", "Age","Gender", "Occupation", "Location", "Average Monthly Revenue",
                          "Number of Children", "Last Transaction Date", "Transaction Type",
                          "Year Agios", "Current Balance", "Status" )
      
      
      # Création de l'objet rhandsontable avec les colonnes sélectionnées et les titres correspondants
      rhandsontable(data[, !colnames(data) %in% c("Year Agios","Status")], columnHeadersFixed = TRUE) %>% 
        hot_col("Gender", renderer = "customRenderer")  
    })
    
    output$downloadCSV <- downloadHandler(
      filename = function() {
        "table_Inactive.csv"
      },
      content = function(file) {
        # Récupérez les données de la table
        data <- hot_to_r(input$Table)
        # Écrivez les données au format CSV
        write.csv(data, file, row.names = FALSE)
      }
    )
    
  })
  
  output$DefaultEvolution <- renderHighchart({
    six_months <- seq(input$dateRangeInput[2], by = "1 month", length.out = 6)
    PD <- c(
      (1-1/(1 + exp(-predict(log_model1, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model2, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model3, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model4, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model5, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model6, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response"))))
    )
    
    data_pd <- data.frame(Month = six_months, PD= round(PD*100,2))
    
    highchart() %>%
      hc_title(text = "Default probabilities over the next six months") %>%
      hc_xAxis(categories = six_months) %>%
      hc_yAxis(
        title = list(text = "Default Probability (%)"),
        labels = list(format = "{value}%")
      ) %>%
      hc_add_series(name="PD", data = data_pd$PD) %>%
      hc_tooltip(
        valueSuffix = "%"
      )
  })
  
  output$Table1 <- renderFormattable({
    
    # Exemple de données pour le tableau
    data= table_predict_data[1:50,]
    
    colnames(data) <- c("Client_id", "First Name", "Last Name", "Age","Gender", "Occupation", "PD month1", "PD month2",
                        "PD month3", "PD month4", "PD month5","PD month6")
    
    formattable(data, align = "l")
  })
  
  output$downloadCSV1 <- downloadHandler(
    filename = function() {
      "table_default_predictive.csv"
    },
    content = function(file) {
      # Récupérez les données de la table
      data <- hot_to_r(input$Table1)
      # Écrivez les données au format CSV
      write.csv(data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)