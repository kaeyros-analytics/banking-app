function(input, output) {
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
      filter(if (input$statusFilter != "All") status == input$statusFilter else TRUE)
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
    temporale_data <- as.data.frame(do.call(rbind, lapply(names(dictionnaire), function(key) {
      c(date = key, number_Inactive = dictionnaire[[key]][1], number_Active = dictionnaire[[key]][2])
    })))
    # Spécifier le type de données pour les colonnes
    temporale_data$date <- as.Date(paste(temporale_data$date, "01", sep = "."), format = "%b.%Y.%d")
    temporale_data$number_Inactive <- as.integer(temporale_data$number_Inactive)
    temporale_data$number_Active <- as.integer(temporale_data$number_Active)
    
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
    output$totalCustomersByGender <- renderPlotly({
      # Données
      df1 <- totalCustomers_df %>%
        select(account_id, gender) %>%
        distinct(account_id, gender, .keep_all = TRUE) %>%
        group_by(gender) %>%
        summarise(total = n()) %>%
        mutate(percentage = round((total / sum(total)) * 100, 2)) # Calcul du pourcentage
      
      plot_ly(df1, labels = ~gender, values = ~total, type = "pie", hole = 0.5) %>%
        layout(
          title = "Customers by Gender",
          showlegend = TRUE
        ) %>%
        add_trace(
          textinfo = "percent",
          hoverinfo = "text+percent",
          text = ~paste(gender, ": ", total),
          marker = list(colors = ifelse(df1$gender == "Women", "fuchsia", "skyblue"),
                        line = list(color = "rgba(255,255,255,0.5)", width = 2))
        )
    })
    
    
    output$totalCustomersByMarital <- renderPlotly({
      customers_by_marital <- totalCustomers_df %>%
        select(account_id, marital_status, status) %>%
        distinct(account_id, marital_status, .keep_all = TRUE) %>%
        group_by(marital_status, status) %>%
        summarise(customer_count = n())
      
      plot_ly() %>%
        add_trace(
          x = customers_by_marital$customer_count[customers_by_marital$status == "Inactive"],
          y = customers_by_marital$marital_status[customers_by_marital$status == "Inactive"],
          type = "bar",
          name = "Inactive",
          orientation = "h",
          marker = list(color = "fuchsia")
        ) %>%
        add_trace(
          x = customers_by_marital$customer_count[customers_by_marital$status == "Active"],
          y = customers_by_marital$marital_status[customers_by_marital$status == "Active"],
          type = "bar",
          name = "Active",
          orientation = "h",
          marker = list(color = "skyblue")
        ) %>%
        layout(
          title = "Customers by marital situation",
          xaxis = list(title = "Number of Customers"),
          yaxis = list(title = "Marital Status"),
          barmode = "group",
          legend = list(orientation = "h", traceorder = "reversed",x = 0.25, y = -0.2)
        )
    })
    
    output$CustomersStatusByAgency <- renderPlotly({
      # Calculer le nombre de clients par statut et par agence
      df_status_by_agency <- totalCustomers_df %>%
        select(account_id, location, status) %>%
        distinct(account_id, location, .keep_all = TRUE) %>%
        group_by(location, status) %>%
        summarise(customer_count = n())
      
      # Créer le graphique Plotly
      plot_ly() %>%
        add_trace(
          x = df_status_by_agency$customer_count[df_status_by_agency$status == "Inactive"],
          y = df_status_by_agency$location[df_status_by_agency$status == "Inactive"],
          type = "bar",
          name = "Inactive",
          orientation = "h",
          marker = list(color = "fuchsia")
        ) %>%
        add_trace(
          x = df_status_by_agency$customer_count[df_status_by_agency$status == "Active"],
          y = df_status_by_agency$location[df_status_by_agency$status == "Active"],
          type = "bar",
          name = "Active",
          orientation = "h",
          marker = list(color = "skyblue")
        ) %>%
        layout(
          title = "Customer Status by city",
          xaxis = list(title = "Number of Customers"),
          yaxis = list(title = "Location"),
          barmode = "group",
          legend = list(orientation = "h", traceorder = "reversed",x = 0.25, y = -0.2)
        )
    })
    
    output$CustomersStatusByActivity <- renderPlotly({
      df_status_by_activity <- totalCustomers_df %>%
        select(account_id, occupation, status) %>%
        distinct(account_id, occupation, .keep_all = TRUE) %>%
        group_by(occupation, status) %>%
        summarise(customer_count = n())
      
      plot_ly() %>%
        add_trace(
          x = df_status_by_activity$customer_count[df_status_by_activity$status == "Inactive"],
          y = df_status_by_activity$occupation[df_status_by_activity$status == "Inactive"],
          type = "bar",
          name = "Inactive",
          orientation = "h",
          marker = list(color = "fuchsia")
        ) %>%
        add_trace(
          x = df_status_by_activity$customer_count[df_status_by_activity$status == "Active"],
          y = df_status_by_activity$occupation[df_status_by_activity$status == "Active"],
          type = "bar",
          name = "Active",
          orientation = "h",
          marker = list(color = "skyblue")
        ) %>%
        layout(
          title = "Customer Status by occupation",
          xaxis = list(title = "Number of Customers"),
          yaxis = list(title = "Occupation"),
          barmode = "group",
          legend = list(orientation = "h", traceorder = "reversed",x = 0.25, y = -0.2)
        )
    })
    
    output$CustomersStatusByBalanceClass <- renderPlotly({
      
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
        mutate(percentage = round(count / total_count * 100, 2))
      
      # Créer le graphique Plotly
      plot_ly() %>%
        add_trace(
          x = data_summary$balance_interval[data_summary$status == "Active"],
          y = data_summary$count[data_summary$status == "Active"],
          type = "bar",
          name = "Active",
          marker = list(color = "skyblue")
        ) %>%
        add_trace(
          x = data_summary$balance_interval[data_summary$status == "Inactive"],
          y = data_summary$count[data_summary$status == "Inactive"],
          type = "bar",
          name = "Inactive",
          marker = list(color = "fuchsia")
        ) %>%
        layout(
          title = "Status of customers by balance",
          xaxis = list(title = "Balance Interval"),
          yaxis = list(title = "Number of Customers"),
          barmode = "group",
          legend = list(orientation = "h", x = 0.25, y = -0.2)
        )
    })
    
    output$CustomersStatusByAgeClass <- renderPlotly({
      
      data_summary <- totalCustomers_df %>%
        select(account_id, age_interval, status) %>%
        distinct(account_id, age_interval, .keep_all = TRUE) %>%
        group_by(age_interval, status) %>%
        summarise(count = n())
      
      plot_ly() %>%
        add_trace(
          x = data_summary$age_interval[data_summary$status == "Active"],
          y = data_summary$count[data_summary$status == "Active"],
          type = "bar",
          name = "Active",
          marker = list(color = "skyblue")
        ) %>%
        add_trace(
          x = data_summary$age_interval[data_summary$status == "Inactive"],
          y = data_summary$count[data_summary$status == "Inactive"],
          type = "bar",
          name = "Inactive",
          marker = list(color = "fuchsia")
        ) %>%
        layout(
          title = "Status of customers by age",
          xaxis = list(title = "Age Interval"),
          yaxis = list(title = "Number of Customers"),
          barmode = "group",
          legend = list(orientation = "h", x = 0.25, y = -0.2)
        )
    })
     
    output$TemporelSerie <- renderPlotly({
      
      # Créer une structure de base Plotly
      p <- plot_ly(temporale_data) %>%
        layout(
          title = "Temporal evolution of Inactive accounts",
          xaxis = list(title = "Date", tickformat = "%b.%Y"),
          yaxis = list(title = "Count"),
          legend = list(
            orientation = "h", # horizontal
            x = 0.25, y = -0.2, # position relative au graphique
            bgcolor = "white" # couleur de fond de la légende
          ),
          hovermode = "closest"
        )
      
      # Ajouter les traces en fonction de l'input$statusFilter
      if (input$statusFilter == 'All') {
        p <- p %>%
          add_lines(x = ~date, y = ~number_Inactive, name = "Number of Inactive", line = list(color = "#e80675")) %>% 
          add_lines(x = ~date, y = ~number_Active, name = "Number of Active", line = list(color = "blue"))
      } else if (input$statusFilter=="Inactive") {
        p <- p %>%
          add_lines(x = ~date, y = ~number_Inactive, name = "Number of Inactive", line = list(color = "#e80675"))
      } else if (input$statusFilter=="Active") {
        p <- p %>%
          add_lines(x = ~date, y = ~number_Active, name = "Number of Active", line = list(color = "blue"))
      }
      return(p)
    })
    

    output$Carte <- renderLeaflet({
      df <- totalCustomers_df %>%
        distinct(account_id, latitude,longitude, marital_status, last_name, first_name, age,gender, occupation,location, status) #%>%
      #filter(status == "Inactive")

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
            "<b>Occupation:</b> ", occupation, "<br>",
            "<b>Location:</b> ", location
          ),
          clusterOptions = markerClusterOptions()  # Ajout du regroupement de marqueurs
        ) %>%
        addScaleBar() %>%     # Ajout de l'échelle
        addCircleMarkers(
          color = ~ifelse(status == "Active", "blue", "red")
        ) %>%
        addLegend("topright",
                  colors = c("blue", "red"),
                  labels = c("Active", "Inactive"),
                  title = "Status",
                  opacity = 1) %>%
        setView(lng = 7.284076, lat = 5.054194591, zoom = 6)
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
  },ignoreNULL = FALSE)
  
  output$DefaultEvolution <- renderPlotly({
    six_months <- seq(input$dateRangeInput[2], by = "1 month", length.out = 6)
    PD <- c(
      (1-1/(1 + exp(-predict(log_model1, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model2, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model3, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model4, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model5, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model6, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response"))))
    )

    data_pd <- data.frame(Month = six_months, PD = PD)

    plot_ly() %>%
      layout(
        title = "Inactive probabilities over the next six months",
        xaxis = list(title = "Six Months"),
        yaxis = list(title = "Default Probability (%)", tickformat = ".1%"),
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Arial", size = 12, color = "black")
        )
      ) %>%
      add_trace(
        x = six_months,
        y = data_pd$PD,
        type = "scatter",
        mode = "lines+markers",
        name = "PD",
        line = list(color = "red"),
        text = paste(round(data_pd$PD*100,2),"%"),
        hoverinfo = "text"
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
