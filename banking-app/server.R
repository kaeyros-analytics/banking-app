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
      mutate(status = if_else((as.Date(input$dateRangeInput[2]) - last_transaction_date) > 60, "Dormant", "Operating")) %>% 
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
      nomber_Dormant <- nrow(temporale_df %>% filter(date[[1]] - last_transaction_date > 60))
      key <- format(date[[1]], "%b.%Y",locale=locale)
      value <- c(nomber_Dormant, nrow(temporale_df) - nomber_Dormant)
      dictionnaire[[key]] <- value
    }
    
    # Convertir le dictionnaire en dataframe
    temporale_data <- as.data.frame(do.call(rbind, lapply(names(dictionnaire), function(key) {
      c(date = key, number_Dormant = dictionnaire[[key]][1], number_Operating = dictionnaire[[key]][2])
    })))
    # Spécifier le type de données pour les colonnes
    temporale_data$date <- as.Date(paste(temporale_data$date, "01", sep = "."), format = "%b.%Y.%d")
    temporale_data$number_Dormant <- as.integer(temporale_data$number_Dormant)
    temporale_data$number_Operating <- as.integer(temporale_data$number_Operating)
    
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
    
    output$OperatingCustomers <- renderInfoBox({
      df_Operating <- totalCustomers_df %>%
        filter(status== "Operating")
      
      tags$div(
        infoBox(
          HTML("<strong>Operating Customers</strong>"),
          value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;", 
                         length(unique(df_Operating$account_id))),
          icon = icon("users"),  
          color = "teal",
          width = 12  
        )
      )
    })
    
    output$DormantCustomers <- renderInfoBox({
      df_Dormant <- totalCustomers_df %>%
        filter(status== "Dormant")
      
      tags$div(
        infoBox(
          HTML("<strong>Dormant Customers</strong>"),
          value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;", 
                         length(unique(df_Dormant$account_id))), 
          icon = icon("users"),  
          color = "teal",
          width = 12  
        )
      )
    })
    
    output$revenueLoss <- renderInfoBox({
      
      Dormant_accounts <- totalCustomers_agios_df %>% 
        filter(status == "Dormant",balance_after_agios<0)
      
      # financial impact of inactivity
      tags$div(
        infoBox(
          HTML("<strong>Revenue Loss</strong>"),
          value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;", 
                         paste(-round(sum(Dormant_accounts$balance_after_agios)/1000000,2)),"M"), 
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
          x = customers_by_marital$customer_count[customers_by_marital$status == "Dormant"],
          y = customers_by_marital$marital_status[customers_by_marital$status == "Dormant"],
          type = "bar",
          name = "Dormant",
          orientation = "h",
          marker = list(color = "fuchsia")
        ) %>%
        add_trace(
          x = customers_by_marital$customer_count[customers_by_marital$status == "Operating"],
          y = customers_by_marital$marital_status[customers_by_marital$status == "Operating"],
          type = "bar",
          name = "Operating",
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
          x = df_status_by_agency$customer_count[df_status_by_agency$status == "Dormant"],
          y = df_status_by_agency$location[df_status_by_agency$status == "Dormant"],
          type = "bar",
          name = "Dormant",
          orientation = "h",
          marker = list(color = "fuchsia")
        ) %>%
        add_trace(
          x = df_status_by_agency$customer_count[df_status_by_agency$status == "Operating"],
          y = df_status_by_agency$location[df_status_by_agency$status == "Operating"],
          type = "bar",
          name = "Operating",
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
          x = df_status_by_activity$customer_count[df_status_by_activity$status == "Dormant"],
          y = df_status_by_activity$occupation[df_status_by_activity$status == "Dormant"],
          type = "bar",
          name = "Dormant",
          orientation = "h",
          marker = list(color = "fuchsia")
        ) %>%
        add_trace(
          x = df_status_by_activity$customer_count[df_status_by_activity$status == "Operating"],
          y = df_status_by_activity$occupation[df_status_by_activity$status == "Operating"],
          type = "bar",
          name = "Operating",
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
        summarise(
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
          x = data_summary$balance_interval[data_summary$status == "Operating"],
          y = data_summary$count[data_summary$status == "Operating"],
          type = "bar",
          name = "Operating",
          marker = list(color = "skyblue")
        ) %>%
        add_trace(
          x = data_summary$balance_interval[data_summary$status == "Dormant"],
          y = data_summary$count[data_summary$status == "Dormant"],
          type = "bar",
          name = "Dormant",
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
          x = data_summary$age_interval[data_summary$status == "Operating"],
          y = data_summary$count[data_summary$status == "Operating"],
          type = "bar",
          name = "Operating",
          marker = list(color = "skyblue")
        ) %>%
        add_trace(
          x = data_summary$age_interval[data_summary$status == "Dormant"],
          y = data_summary$count[data_summary$status == "Dormant"],
          type = "bar",
          name = "Dormant",
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
          title = "Temporal evolution of Dormant accounts",
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
          add_lines(x = ~date, y = ~number_Dormant, name = "Number of Dormant", line = list(color = "#e80675")) %>% 
          add_lines(x = ~date, y = ~number_Operating, name = "Number of Operating", line = list(color = "blue"))
      } else if (input$statusFilter=="Dormant") {
        p <- p %>%
          add_lines(x = ~date, y = ~number_Dormant, name = "Number of Dormant", line = list(color = "#e80675"))
      } else if (input$statusFilter=="Operating") {
        p <- p %>%
          add_lines(x = ~date, y = ~number_Operating, name = "Number of Operating", line = list(color = "blue"))
      }
      return(p)
    })
    

    output$Carte <- renderLeaflet({
      df <- totalCustomers_df %>%
        distinct(account_id, latitude,longitude, marital_status, last_name, first_name, age,gender, occupation,location, status) #%>%
      #filter(status == "Dormant")

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
          color = ~ifelse(status == "Operating", "blue", "red")
        ) %>%
        addLegend("topright",
                  colors = c("blue", "red"),
                  labels = c("Operating", "Dormant"),
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
        filter(status=="Dormant")

      colnames(data) <- c("account_id", "First Name", "Last Name", "Age","Gender", "Occupation", "Location", "Average Monthly Revenue",
                          "Number of Children", "Last Transaction Date", "Transaction Type",
                          "Year Agios", "Current Balance", "Status" )


      # Création de l'objet rhandsontable avec les colonnes sélectionnées et les titres correspondants
      rhandsontable(data[, !colnames(data) %in% c("Year Agios","Status")], columnHeadersFixed = TRUE) %>%
        hot_col("Gender", renderer = "customRenderer")
    })

    output$downloadCSV <- downloadHandler(
      filename = function() {
        "table_Dormant.csv"
      },
      content = function(file) {
        # Récupérez les données de la table
        data <- hot_to_r(input$Table)
        # Écrivez les données au format CSV
        write.csv(data, file, row.names = FALSE)
      }
    )
  },ignoreNULL = FALSE)
  
  output$DefaultEvolutionClient <- renderPlotly({
    six_months <- seq(input$dateRangeInput[2], by = "3 month", length.out = 3)
    PD <- c(
      #(1-1/(1 + exp(-predict(log_model1, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      #(1-1/(1 + exp(-predict(log_model2, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      #(1-1/(1 + exp(-predict(log_model3, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model4, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model5, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response")))),
      (1-1/(1 + exp(-predict(log_model6, newdata = predict_data[predict_data$Client_id == input$client_id, ][,1:6], type = "response"))))
    )

    data_pd <- data.frame(Month = six_months, PD = PD)

    plot_ly() %>%
      layout(
        title = "Dormant probabilities over the next six months",
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
    six_months <- seq(input$dateRangeInput[2], by = "3 month", length.out = 3)
    
    # Exemple de données pour le tableau
    data= table_predict_data[1:50,c(1:6,10:12)]

    colnames(data)[1:6] <- c("Client_id", "First Name", "Last Name", "Age","Gender", "Occupation")
                             #, "PD month1", "PD month2","PD month3", "PD month4", "PD month5","PD month6")
    colnames(data)[7:9] <- format(six_months, "%b.%Y")
    formattable(data, align = c("l",rep("r", NCOL(data) - 1)), 
                list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                     area(col = 7:9) ~ function(x) percent(x, digits = 0),
                     area(col = 7:9) ~ color_tile("#DeF7E9", "#71CA97")))
    
    
    #formattable(data, align = "l")
    
    
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
  
  output$FeatureImportance <- renderPlotly({
    plot_ly(coef_df, x = ~Coefficient, y = ~Feature, type = "bar", orientation = 'v', 
            marker = list(color = ~Coefficient, colorscale = 'Viridis')) %>%
      layout(title = "Variables influencing the dormant state of accounts",
             yaxis = list(title = "Coefficient"),
             xaxis = list(title = "Impact of influence"))
  })
  
  output$LossEvolution <- renderPlotly({
    months_past <- seq(input$dateRangeInput[2] %m-% months(24), by = "1 month", length.out = 24)
    months_future <- seq(input$dateRangeInput[2] %m+% months(0), by = "1 month", length.out = 9)
    
    loss <- c()
    for (month in months_past){
      Dormant_accounts <- inner_account_trans_agios %>% 
        filter(year_agios <= as.Date(month) ,balance_after_agios<0)
      loss <- c(loss,-round(sum(Dormant_accounts$balance_after_agios)/100000000,2) )
    }
    loss <- sample(x = loss, size = length(loss), replace = TRUE)
    #   rnorm(24, mean = mean(loss), sd = sd(loss))
    ts_data=ts(loss)
    fit <- auto.arima(ts_data)
    forecast_values <- forecast(fit,h=9)
    # Nombre de périodes à prédire
    n_pred <- 9
    loss_data <- data.frame(
      time = c(months_past, months_future),
      Loss = c(loss,head(forecast_values$mean,1) ,rep(NA, n_pred-1)),
      Loss_predict = c(rep(NA, length(loss)),forecast_values$mean),
      Lower = c(rep(NA, length(loss)),head(forecast_values$mean,1), forecast_values$lower[1:8]),
      Upper = c(rep(NA, length(loss)),head(forecast_values$mean,1), forecast_values$upper[1:8]),
      Type = rep(c("Actual", "Forecast"), c(length(loss), n_pred))
    )
    
    plot_ly(loss_data, x = ~time) %>%
      add_lines(y = ~Loss, name = "Loss", line = list(color = "blue")) %>%
      add_lines(y = ~Loss_predict, name = "Loss Prediction", line = list(color = "red")) %>%
      add_ribbons(ymin = ~Lower, ymax = ~Upper, name = "Prediction Interval", fill = "rgba(0,100,80,0.2)") %>%
      layout(title = "Loss Prediction vs Actual Loss in Agios",
             xaxis = list(title = "Time", tickformat = "%b %Y"),
             yaxis = list(title = "Loss(Million)", tickformat = ",.2r", ticksuffix = "M"))
    
    
  }) 
  output$RiskIntensitybyAge <- renderPlotly({
    # Définir une palette de couleurs Viridis
    palette_colors <- viridisLite::viridis(4)
    
    # Créer le graphique à partir des données risk_data avec la palette de couleurs Viridis
    plot_ly(risk_data, x = ~risk_intensity/100, y = ~age_group, 
            color = ~risk_quality, colors = palette_colors, 
            type = "bar", orientation = "h") %>%
      layout(title = "Dormant Risk Intensity by Age Group",
             xaxis = list(title = "Risk Intensity (%)", tickformat=".1%"),
             yaxis = list(title = "Age Group"),
             barmode = "stack", # Utilisation de barmode = "stack" pour empiler les barres
             legend = list(title = "Risk Quality"))
    
    
  }) 
  output$RiskIntensitybyBalance <- renderPlotly({
    plot_ly(risk_data_balance, x = ~risk_intensity_balance/100, y = ~current_balance_risk, 
            color = ~risk_quality, colors = palette_colors, 
            type = "bar", orientation = "h") %>%
      layout(title = "Dormant Risk Intensity by current balance",
             xaxis = list(title = "Risk Intensity (%)", tickformat = ".1%"),
             yaxis = list(title = "current balance Group"),
             barmode = "stack", # Utilisation de barmode = "stack" pour empiler les barres
             legend = list(title = "Risk Quality"))
  })
}
