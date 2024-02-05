
totalCustomers_df <- reactive({
  
  if(input$genderFilter == "All"){
    account %>%
      full_join(transactions, by = "account_id") %>% 
      filter(transaction_date >= input$dateRangeFilter[1],
             transaction_date <= input$dateRangeFilter[2],
             #if () gender == input$genderFilter else TRUE,
             status == input$statusFilter,
             transaction_city == input$cityFilter)
  } else{
    account %>%
      full_join(transactions, by = "account_id") %>% 
      filter(transaction_date >= input$dateRangeFilter[1] &  transaction_date <= input$dateRangeFilter[2],
             gender == input$genderFilter,
             status == input$statusFilter,
             transaction_city == input$cityFilter)
  }

})

df_join_full = account %>%
  full_join(transactions, by = "account_id",relationship = "many-to-many")
View(df_join_full)

df_join_left = account %>%
  left_join(transactions, by = "account_id",relationship = "many-to-many") %>% 
  filter(transaction_date >= "2016-09-22" &  transaction_date <= "2022-09-22",
         status == "Inactive")
  
nrow(df_join_left)

# transactions_type_df <- reactive({
#   transactions %>%
#     filter(transaction_date >= input$dateRangeFilter[1],
#            transaction_date <= input$dateRangeFilter[2],
#            if (input$genderFilter != "All") gender == input$genderFilter else TRUE,
#            status == input$statusFilter,
#            city == input$cityFilter)
# })



#   output$activeCustomers <- renderText({
#     paste("Active Customers: ", sum(totalCustomers_df$status == "Active"))
#   })
#   
#   output$inactiveCustomers <- renderText({
#     paste("Inactive Customers: ", sum(totalCustomers_df$status == "Inactive"))
#   })
#   
  # # Additional Analysis
  # output$totalCustomersByMarital <- renderPlot({
  #   ggplot(totalCustomers_df, aes(x = marital_status, fill = marital_status)) +
  #     geom_bar() +
  #     labs(title = "Total Customers by Marital Status")
  # })
#   
#   output$totalCustomersByGender <- renderPlot({
#     ggplot(totalCustomers_df, aes(x = gender, fill = gender)) +
#       geom_bar() +
#       labs(title = "Total Customers by Gender")
#   })
#   
#   output$transactionsByType <- renderPlot({
#     ggplot(transactions_type_df, aes(x = transaction_type, fill = transaction_type)) +
#       geom_bar() +
#       labs(title = "Number of Transactions by Type")
#   })
#   
#   output$transactionsByGender <- renderPlot({
#     ggplot(df_transactions_gender, aes(x = gender, fill = transaction_type)) +
#       geom_bar(position = "dodge") +
#       labs(title = "Number of Transactions by Gender")
#   })


test <- account %>%
  full_join(transactions, by = "account_id")

df <- test %>%
  filter(transaction_date >= "2021-09-22",  
         transaction_date <= "2022-09-22")#,
         #gender == "Femme",
         #transaction_city == "Douala")



test <- as.data.frame(test)
typeof(test$transaction_date)

 x <- lubridate::as_date(test$transaction_date)
typeof(x)
min(x)


df_active <- df %>%
  filter(status== "Active")
length(unique(df_active$account_id))
table(df$status)


ggplot(df %>% 
         select(account_id,marital_status) %>%
         distinct(account_id, marital_status, .keep_all = TRUE), aes(x = marital_status, fill = marital_status)) +
  geom_bar() + coord_flip() +
  labs(title = "Total Customers by Marital Status")

View(df %>% 
  select(account_id,marital_status) %>%
    distinct(account_id, marital_status, .keep_all = TRUE))

df1 <- df %>%
  select(account_id, marital_status) %>%
  distinct(account_id, marital_status, .keep_all = TRUE) %>%
  group_by(marital_status) %>%
  summarise(total = n())
highchart() %>%
  hc_chart(type = "bar") %>%
  hc_xAxis(categories = df1$marital_status) %>%
  hc_yAxis(title = list(text = "Total Customers")) %>%
  hc_add_series(name = "Total Customers", data = df1$total) %>%
  hc_title(text = "Total Customers by Marital Status") %>%
  hc_plotOptions(series = list(stacking = "normal"))



View(df %>% 
       select(transaction_id,transaction_type) %>%
       distinct(transaction_id,transaction_type, .keep_all = TRUE))


test1 <- transactions %>% 
  filter(transaction_date <= "2022-09-22")

test2 <- account %>% 
  filter(gender == "Homme")

inner_test <- test2 %>% 
  inner_join(test1, by = "account_id") %>% 
  group_by(account_id) %>%
  mutate(last_transaction_date = max(transaction_date))

inner_test_filter <- inner_test %>% 
  filter(last_transaction_date >= "2021-09-22" & last_transaction_date <= "2022-09-22") %>% 
  mutate(status = if_else((as.Date("2022-09-22") - last_transaction_date) > 60, "Dormant", "Operating")) %>% 
  filter( transaction_city == "Douala")

# Analyse du sexe des comptes
ggplot(data = inner_test_filter, aes(x = gender, fill = status)) +
  geom_bar(position = "dodge") +
  labs(x = "Sexe", y = "Nombre de comptes", fill = "Statut du compte") +
  ggtitle("Répartition du sexe des comptes par statut")

ggplot(data = inner_test_filter, aes(x = marital_status, fill = status)) +
  geom_bar(position = "dodge") +
  labs(x = "Situation matrimoniale", y = "Nombre de comptes", fill = "Statut du compte") +
  ggtitle("Répartition de la situation matrimoniale des comptes par statut")



inner_test_filter1 <- transactions %>%
  filter(transaction_date <= "2022-09-22") %>%
  inner_join(account %>% filter(gender == "Homme"), by = "account_id") %>%
  group_by(account_id) %>%
  mutate(
    last_transaction_date = max(transaction_date),
    status = if_else((as.Date("2022-09-22") - last_transaction_date) > 180, "Dormant", "Operating")
  ) %>%
  filter(last_transaction_date >= "2021-09-22" & last_transaction_date <= "2022-09-22" & transaction_city == "Douala")

test3 <- inner_test_filter %>%
  select(account_id,transaction_agency, status) %>%
  distinct(account_id,transaction_agency, .keep_all = TRUE) %>%
  group_by(transaction_agency, status) %>%
  summarise(customer_count = n())


test_agios <- agios %>% 
  filter(year_agios<= "2022-09-22") 

inner_test_filter_agios <- inner_test_filter %>% 
  inner_join(test_agios,by="account_id",
             relationship = "many-to-many")


df1 <- inner_test_filter %>%
  select(account_id, gender) %>%
  distinct(account_id, gender, .keep_all = TRUE) %>%
  group_by(gender) %>%
  summarise(total = n())

highchart() %>%
  hc_chart(type = "pie") %>%
  hc_add_series(name = "Customers", data = list_parse2(df1),innerSize = "50%") %>%
  hc_title(text = "Customers by Gender")


df2 <- inner_test_filter_agios %>% 
  select(account_id,balance_after_agios, status,year_agios) %>% 
  group_by(account_id) %>% 
  group_by(account_id) %>% 
  summarize(max_year_agios = max(year_agios),
            balance_after_agios = balance_after_agios[which.max(year_agios)],
            status = status[which.max(year_agios)])


# Filtrer les comptes dormants
dormant_accounts <- inner_test_filter_agios %>% 
  filter(status == "Dormant",
         balance_after_agios<0)
sum(dormant_accounts$balance_after_agios) # financial impact of inactivity


# Créer un histogramme des montants d'agios accumulés
ggplot(data = dormant_accounts, aes(x = agios)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(x = "Montant des agios", y = "Nombre de comptes", title = "Distribution des montants d'agios sur les comptes dormants") +
  theme_minimal()


ggplot(df2, aes(x = balance_after_agios)) +
  geom_boxplot() +  
  labs(title = "Distribution des soldes après agios",
       x = "Solde après agios",
       y = "Fréquence")


quartiles <- quantile(df2$balance_after_agios, probs = c(0.25, 0.5, 0.75))
quartiles
# Affichage des quartiles
print(quartiles)

# Spécifier les intervalles personnalisés
intervals <- c(-Inf, 0, round(quartiles[1], -5), round(quartiles[1], -5), round(quartiles[1], -5), Inf)

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
  hc_title(text = "Statut des clients par intervalle de solde") %>%
  hc_xAxis(categories = unique(data_summary$balance_interval)) %>%
  hc_yAxis(title = list(text = "Nombre de clients")) %>%
  hc_plotOptions(grouping = FALSE) %>%
  hc_add_series(name = "Dormant", data = data_summary[data_summary$status == "Dormant",]$count) %>%
  hc_add_series(name = "Operating", data = data_summary[data_summary$status == "Operating",]$count) %>%
  hc_legend(enabled = TRUE)



# Analyse du sexe des comptes
ggplot(data = donnees_comptes, aes(x = sexe, fill = status)) +
  geom_bar(position = "dodge") +
  labs(x = "Sexe", y = "Nombre de comptes", fill = "Statut du compte") +
  ggtitle("Répartition du sexe des comptes par statut")


account_filter_by_gender <- account #%>% 
  #filter(gender == "Homme")

transactions_filter_by_date2 <- transactions %>% 
  filter(transaction_date <= "2022-09-22") %>% 
  filter(transaction_city == "Douala")

# Jointure interne des comptes et des transactions filtrées par la date
inner_account_transactions <- account_filter_by_gender %>% 
  inner_join(transactions_filter_by_date2, by = "account_id",relationship = "many-to-many") %>% 
  group_by(account_id) %>%
  # Calcul de la date de la dernière transaction pour chaque compte
  mutate(last_transaction_date = max(transaction_date, na.rm = TRUE))


date1 <- as.Date("2018-02-11")
date2 <- as.Date("2019-11-22")
sequence_dates <- seq(date1, date2, by = "month")
# Arrondir chaque date au dernier jour du mois
sequence_dates_arrondies <- lapply(sequence_dates, function(date) {
  ceiling_date(date, unit = "month") - days(1)
})



test4<- inner_account_transactions %>% 
  select(account_id,last_transaction_date,transaction_city) %>% 
  filter(if (input$cityFilter != "All") transaction_city == input$cityFilter else TRUE) %>% 
  distinct(account_id,last_transaction_date)


date1 <- as.Date("2022-02-11")
date2 <- as.Date("2023-11-22")
sequence_dates <- seq(date1, date2, by = "month")
# Arrondir chaque date au dernier jour du mois
sequence_dates_arrondies <- lapply(sequence_dates, function(date) {
  ceiling_date(date, unit = "month") - days(1)
})

dictionnaire <- list()  # Initialisation du dictionnaire
locale <- Sys.setlocale("LC_TIME", "en_GB.UTF-8")
for (date in sequence_dates_arrondies) {
  nomber_dormant <- nrow(test4 %>% filter(date[[1]] - last_transaction_date > 60))
  key <- format(date[[1]], "%b.%Y",locale=locale)
  value <- c(nomber_dormant, nrow(test4) - nomber_dormant)
  dictionnaire[[key]] <- value
}

print(dictionnaire)

# Convertir la matrice en dataframe
df <- as.data.frame(do.call(rbind, lapply(names(dictionnaire), function(key) {
  c(date = key, number_dormant = dictionnaire[[key]][1], number_operating = dictionnaire[[key]][2])
})))

# Spécifier le type de données pour les colonnes
df$number_dormant <- as.integer(df$number_dormant)
df$number_operating <- as.integer(df$number_operating)

# Afficher le dataframe
print(df)

# Charger la bibliothèque ggplot2 si ce n'est pas déjà fait
library(ggplot2)
# Créer un graphique Highchart en fonction de input$statut
highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(text = "Séries Temporelles") %>%
  hc_xAxis(categories = as.character(df$date), labels = list(format = "{value:%b.%Y}")) %>%
  hc_yAxis(title = list(text = "Count")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(shared = TRUE, crosshairs = TRUE, valueDecimals = 0) %>%
  hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
  hc_credits(enabled = FALSE) %>%
  # Ajouter la série selon input$statut
  if (input$statut == "Dormant") {
    hc_add_series(data = df$number_dormant, name = "Number of Dormant", color = "red")
  } else if (input$statut == "Operating") {
    hc_add_series(data = df$number_operating, name = "Number Operating", color = "blue")
  } else {
    hc_add_series(data = df$number_dormant, name = "Number of Dormant", color = "red") %>%
      hc_add_series(data = df$number_operating, name = "Number Operating", color = "blue")
  }


view(account$geo_location)


latitude = c()
longitude = c()
account_identity = c()
for (a in  account$geo_location){
  latitude<- c(latitude,a[1])
  longitude <- c(longitude,a[2])
}


# Création d'un exemple de jeu de données avec des coordonnées géographiques
data <- data.frame(
  account_id = account$account_id,
  latitude = latitude,
  longitude = longitude
)

# Création de la carte Leaflet
mymap <- leaflet(data) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, popup = ~as.character(account_id))

# Affichage de la carte
mymap


df <- inner_account_transactions %>% 
  distinct(account_id,geo_location,marital_status,last_name,first_name,age,occupation) %>% 
  filter(marital_status=="Veuf")


df_status_by_agency <- account_filter_by_gender %>%
  select(account_id,location, gender) %>%
  distinct(account_id,location, .keep_all = TRUE) %>%
  group_by(location, gender) %>%
  summarise(customer_count = n())

# Créer le graphique Highchart
highchart() %>%
  hc_chart(type = "bar") %>%
  hc_xAxis(categories = df_status_by_agency$location) %>%
  hc_add_series(
    name = "Active", 
    data = df_status_by_agency %>%
      filter(gender == "Men") %>%
      pull(customer_count)
  ) %>%
  hc_add_series(
    name = "Inactive", 
    data = df_status_by_agency %>%
      filter(gender == "Women") %>%
      pull(customer_count)
  ) %>%
  hc_title(text = "Customer Status by Agency") %>%
  hc_yAxis(title = list(text = "Number of Customers")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_plotOptions(grouping = FALSE)


library(ggplot2)

# Diagramme en boîte pour l'âge par rapport à status
ggplot(data1, aes(x = status, y = age, fill = status)) +
  geom_boxplot() +
  labs(title = "Distribution de l'âge par rapport à Status")

# Diagramme de densité pour le revenu mensuel moyen par rapport à status
ggplot(data1, aes(x = average_monthly_revenue, fill = status)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Distribution du revenu mensuel moyen par rapport à Status")

# Diagramme en barres pour le genre par rapport à status
ggplot(data1, aes(x = gender, fill = status)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution du genre par rapport à Status")

# Diagramme en nuage de points pour occupation par rapport à status
ggplot(data1, aes(x = occupation, fill = status)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution de l'occupation par rapport à Status")

# Diagramme en nuage de points pour occupation par rapport à status
ggplot(data1, aes(x = location, fill = status)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution de l'occupation par rapport à Status")

dashboardSidebar(
  selectInput("clientID", "Client ID", c(unique(predict_data$client))),
  selectInput("horizon", "Horizon:", 1:6),
  actionButton("calculate", "Calculate")
)


start_date <- as.Date("2024-01-01")

# Calcul des six prochains mois
six_months <- seq(start_date, by = "1 month", length.out = 6)

# Création d'un data frame pour stocker les données des six prochains mois


# Calcul des PD pour chaque modèle et chaque mois

PD <- c(
  1/(1 + exp(-predict(log_model1, newdata = predict_data[predict_data$client == "client 1", ][,1:6], type = "response"))),
  1/(1 + exp(-predict(log_model2, newdata = predict_data[predict_data$client == "client 1", ][,1:6], type = "response"))),
  1/(1 + exp(-predict(log_model3, newdata = predict_data[predict_data$client == "client 1", ][,1:6], type = "response"))),
  1/(1 + exp(-predict(log_model4, newdata = predict_data[predict_data$client == "client 1", ][,1:6], type = "response"))),
  1/(1 + exp(-predict(log_model5, newdata = predict_data[predict_data$client == "client 1", ][,1:6], type = "response"))),
  1/(1 + exp(-predict(log_model6, newdata = predict_data[predict_data$client == "client 1", ][,1:6], type = "response")))
  )
data_pd <- data.frame(Month = six_months, PD= round(PD*100,2))


# Création du graphique Highcharter
highchart() %>%
  hc_title(text = "Probabilités de défaut pour les six prochains mois") %>%
  hc_xAxis(categories = six_months) %>%
  hc_yAxis(
    title = list(text = "Probabilité de défaut (PD)"),
    labels = list(format = "{value}%")
  ) %>%
  hc_add_series(name = "Mois 1", data = data_pd$PD) %>%
  hc_tooltip(
    valueSuffix = "%"
  )

data <- table_predict_data[1:50,]

colnames(data) <- c("Client_id", "First Name", "Last Name", "Age", "Gender", "Occupation", "PD month1", "PD month2",
                    "PD month3", "PD month4", "PD month5", "PD month6")

# Calculer les variations pour chaque mois
for (i in 7:12) {
  data[[paste0("variation_month", i - 6)]] <- data[[i]] - data[[i - 1]]
}

# Formater les variations
for (i in 13:18) {
  data[[i]] <- color_tile("white", "green")(sign(data[[i]]) * 100 * abs(data[[i]]))
}

formattable::formattable(data, align = "l")



# Charger la bibliothèque formattable
library(formattable)

# Créer un data frame avec les données des clients
data <- data.frame(
  Client = c("client 1", "client 2", "client 3", "client 4", "client 5", "client 6"),
  PD_month1 = c(0.48, 0.46, 0.33, 0.32, 0.27, 0.32),
  PD_month2 = c(0.48, 0.46, 0.33, 0.32, 0.27, 0.32),
  PD_month3 = c(0.48, 0.46, 0.33, 0.32, 0.27, 0.32),
  PD_month4 = c(0.48, 0.46, 0.33, 0.32, 0.27, 0.32),
  PD_month5 = c(0.31, 0.36, 0.28, 0.28, 0.23, 0.44),
  PD_month6 = c(0.52, 0.57, 0.32, 0.28, 0.19, 0.47)
)

dat <- sapply(data[,2:6], as.numeric)

# Créer une matrice pour les différences entre les mois
diff_matrix <- sapply(1:ncol(dat), function(i) {
  dat[, i] - dat[, i - 1]
})

# Ajouter la première colonne (clients) à la matrice des différences
diff_matrix <- cbind(data$Client, diff_matrix)

# Nommer les colonnes
colnames(diff_matrix) <- colnames(data)

# Créer un objet formattable
formattable_diff <- formattable(diff_matrix)

# Afficher le formattable
formattable_diff






