

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
  filter(gender == "Men")

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
  filter(Year_agios<= "2023-12-22") 

inner_test_filter_agios <- inner_test_filter %>% 
  inner_join(test_agios,by="account_id",
             relationship = "many-to-many")

# Définir la date de référence
date_reference <- as.Date("2022-09-22")

# Calculer les 12 mois passés
months_past <- seq(date_reference %m-% months(24), by = "1 month", length.out = 24)

# Calculer les six prochains mois
months_future <- seq(date_reference %m+% months(0), by = "1 month", length.out = 6)

# Afficher les résultats
print("12 mois passés :")
print(months_past)

loss <- c()
for (month in months_past){
  Inactive_accounts <- inner_test_filter_agios %>% 
    filter(Year_agios <= as.Date(month) ,balance_after_agios<0)
  loss <- c(loss,-round(sum(Inactive_accounts$balance_after_agios)/1000000,2) )
}
loss <- sample(x = loss, size = length(loss), replace = TRUE)
#   rnorm(24, mean = mean(loss), sd = sd(loss))
ts_data=ts(loss)

# # Créer un modèle ARIMA à partir des données de séries temporelles ts_data
fit <- auto.arima(ts_data)
plot(forecast(fit,h=20))
autoplot(forecast(fit,h=20)) + xlab("Time") + ylab("Values") + ggtitle("Dynamic Forecast Plot")
# Générer des prévisions pour les 10 prochaines périodes
forecast_values <- sample(x = loss, size = 8, replace = TRUE)
forecast_values <- forecast(fit,h=6)

# Nombre de périodes à prédire
n_pred <- 6
# forecast_values <- rnorm(forecast_values$mean, mean = mean(forecast_values$mean), sd = sd(forecast_values$mean))

# Créer un dataframe pour la visualisation
loss_data <- data.frame(
  time = c(months_past, months_future),
  Loss = c(loss,head(forecast_values$mean,1) ,rep(NA, n_pred-1)),
  Loss_predict = c(rep(NA, length(loss)),forecast_values$mean),
  Lower = c(rep(NA, length(loss)),head(forecast_values$mean,1), forecast_values$lower[1:5]),
  Upper = c(rep(NA, length(loss)),head(forecast_values$mean,1), forecast_values$upper[1:5]),
  Type = rep(c("Actual", "Forecast"), c(length(loss), n_pred))
)

plot_ly(loss_data, x = ~time) %>%
  add_lines(y = ~Loss, name = "Loss", line = list(color = "blue")) %>%
  add_lines(y = ~Loss_predict, name = "Loss Prediction", line = list(color = "red")) %>%
  add_lines(y = ~Lower, name = "Lower", line = list(color = "green")) %>%
  add_lines(y = ~Upper, name = "Upper", line = list(color = "green")) %>%
  layout(title = "Loss Prediction vs Actual Loss",
         xaxis = list(title = "Time", tickformat = "%b %Y"),
         yaxis = list(title = "Loss(Million)",tickformat = ",.2r",ticksufix = "M"))


# Convertir en format plotly
loss_plot <- plot_ly(data = loss_data, x = ~Date, y = ~Loss, type = 'scatter', mode = 'lines', name = 'Loss') %>%
  add_trace(y = ~Upper, fill = 'tonexty', fillcolor = 'rgba(255,0,0,0.2)', line = list(color = 'transparent'), name = 'Upper Bound') %>%
  add_trace(y = ~Lower, fill = 'tonexty', fillcolor = 'rgba(255,0,0,0.2)', line = list(color = 'transparent'), name = 'Lower Bound') %>%
  layout(title = "Pertes financières en banque dues au churn avec prédiction et intervalle de confiance",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Pertes financières"))

# Afficher le graphique
loss_plot

set.seed(123)
ts_data <- ts(rnorm(100), start = c(2020, 1), frequency = 12)

# Fit ARIMA model to the data
fit <- auto.arima(ts_data)

# Generate forecast for the next 20 periods
forecast_values <- forecast(fit, h = 20)

# Extract time series for plotting
time_series <- c(time(ts_data), rep(NA, 20))
forecast_series <- c(ts_data, forecast_values$mean)
lower_bound <- c(ts_data, forecast_values$lower[1:20])
upper_bound <- c(ts_data, forecast_values$uppe[1:20])

# Create a data frame for plotting
df <- data.frame(Time = time_series, Values = forecast_series, Lower = lower_bound, Upper = upper_bound)



time<- seq(as.Date("2022-01-01"), by = "month", length.out = 10)
lower<- c(NA,NA,NA,NA,8.07,5.00,5.43,5.76,6.02,6.22)
loss<- c(3.75,11.89,15.03,4.98,8.07,NA,NA,NA,NA,NA)
loss_predict<- c(NA,NA,NA,NA,8.07,8.08,8.08,8.08,8.08,8.08)
upper<- c(NA,NA,NA,NA,8.07,11.17,10.73,10.41,10.15,9.94)
data = data.frame(time,lower,loss,loss_predict,upper)

# Création du graphique avec Plotly





# Create dynamic forecast plot using Plotly
plot_ly(df, x = ~Time) %>%
  add_lines(y = ~Values, name = 'Forecast', line = list(color = 'blue')) %>%
  add_lines(y = ~Lower, name = 'Lower Bound', line = list(color = 'lightblue')) %>%
  add_lines(y = ~Upper, name = 'Upper Bound', line = list(color = 'lightblue')) %>%
  add_lines(y = ~ts_data, name = 'Actual', line = list(color = 'red')) %>%
  layout(title = 'Dynamic Forecast Plot', xaxis = list(title = 'Time'), yaxis = list(title = 'Values'))

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
library(plotly)
p <- plot_ly() %>%
  layout(
    title = "Temporal evolution of Inactive accounts",
    xaxis = list(title = "Date", tickformat = "%b.%Y"),
    yaxis = list(title = "Count"),
    legend = list(
      orientation = "h", # horizontal
      x = 0.5, y = -0.2, # position relative au graphique
      bgcolor = "white" # couleur de fond de la légende
    ),
    hovermode = "closest"
  ) %>% 
  add_trace(x = df$date, y = df$number_dormant, name = "Number of Inactive", type = "scatter", mode = "lines", line = list(color = "black")) %>%
  add_trace(x = df$date, y = df$number_operating, name = "Number Active", type = "scatter", mode = "lines", line = list(color = "blue"))
print(p)  



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



library(plotly)

# Création de données de démonstration
set.seed(123)
time_series <- seq(as.Date("2022-01-01"), by = "month", length.out = 24)
values <- sin(seq(0, 2*pi, length.out = 24)) + rnorm(24)

# Fit d'un modèle de séries chronologiques
fit <- auto.arima(values)

# Prévisions
forecast_values <- forecast(fit, h = 12)

# Création de données de démonstration
set.seed(123)
time_series <- seq(as.Date("2022-01-01"), by = "month", length.out = 12)
values <- sin(seq(0, 2*pi, length.out = 12)) + rnorm(12)

# Fit d'un modèle de séries chronologiques
fit <- auto.arima(values)

# Prévisions
forecast_values <- forecast(fit, h = 12)

# Ajustement de time_series pour correspondre à la longueur de values
time_series <- seq(as.Date(tail(time_series, 1)), by = "month", length.out = 24)

# Création du graphique avec Plotly
plot_ly() %>%
  add_lines(x = time_series, y = values, name = "Actual") %>%
  add_lines(x = forecast_values$x, y = forecast_values$mean, name = "Forecast") %>%
  layout(title = "Time Series Forecast",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Values"))



time<- seq(as.Date("2022-01-01"), by = "month", length.out = 10)
lower<- c(NA,NA,NA,NA,8.07,5.00,5.43,5.76,6.02,6.22)
loss<- c(3.75,11.89,15.03,4.98,8.07,NA,NA,NA,NA,NA)
loss_predict<- c(NA,NA,NA,NA,8.07,8.08,8.08,8.08,8.08,8.08)
upper<- c(NA,NA,NA,NA,8.07,11.17,10.73,10.41,10.15,9.94)
data = data.frame(time,lower,loss,loss_predict,upper)

# Création du graphique avec Plotly
plot_ly(data, x = ~time) %>%
  add_lines(y = ~loss, name = "Loss", line = list(color = "blue")) %>%
  add_lines(y = ~loss_predict, name = "Loss Prediction", line = list(color = "red")) %>%
  add_lines(y = ~lower, name = "Lower", line = list(color = "green")) %>%
  add_lines(y = ~upper, name = "Upper", line = list(color = "green")) %>%
  layout(title = "Loss Prediction vs Actual Loss",
         xaxis = list(title = "Time", tickformat = "%b %Y"),
         yaxis = list(title = "Loss(Million)",tickformat = ",.2r",ticksufix = "M"))


### groupe de population à risque
age_group<-c(rep("<30",4),rep("30-39",4),rep("40-49",4), rep("50-59",4), rep(">=60",4))
risk_intensity<- c(c(71.96,28.04,0,0),c(69.37,21.52,9.11,0),c(52.6,23.53,12,11.9),c(68.27,20.36,6.37,5.0),c(54.3,26.15,12.97,6.6))
risk_quality<- rep(c("none","low","medium","high"),5)
risk_data <- data.frame(age_group,risk_intensity,risk_quality)




# Définir une palette de couleurs Viridis
palette_colors <- viridisLite::viridis(4)

# Spécifier l'ordre des niveaux de la variable age_group
risk_data$risk_quality <- factor(risk_data$risk_quality, levels = c("none", "low", "medium", "high"))
risk_data$age_group <- factor(risk_data$age_group, levels = c("<30","30-39","40-49", "50-59", ">=60"))

# Créer le graphique à partir des données risk_data avec la palette de couleurs Viridis
plot_ly(risk_data, x = ~risk_intensity/100, y = ~age_group, 
        color = ~risk_quality, colors = palette_colors, 
        type = "bar", orientation = "h") %>%
  layout(title = "Dormant Risk Intensity by Age Group",
         xaxis = list(title = "Risk Intensity (%)", tickformat=".1%"),
         yaxis = list(title = "Age Group"),
         barmode = "stack", # Utilisation de barmode = "stack" pour empiler les barres
         legend = list(title = "Risk Quality"))



# Données pour le groupe de population à risque avec la variable current_balance_risk
current_balance_risk <- rep(c("<250K", "250K-500K", "500K-750K", ">750K"), each = 4)
risk_intensity_balance <- c(c(39.7, 25.3, 20, 15), c(56.18, 21.52, 12.3, 10), c(64.17, 23.53, 7, 5.3), c(62.77, 27.36, 6.37, 3.5))
risk_quality <- rep(c("none", "low", "medium", "high"), 4)

# Création du dataframe pour le groupe de population à risque avec la variable current_balance_risk
risk_data_balance <- data.frame(current_balance_risk, risk_intensity_balance, risk_quality)

# Définir une palette de couleurs Viridis
palette_colors <- viridisLite::viridis(4)

# Spécifier l'ordre des niveaux des variables
risk_data_balance$risk_quality <- factor(risk_data_balance$risk_quality, levels = c("none", "low", "medium", "high"))
risk_data_balance$current_balance_risk <- factor(risk_data_balance$current_balance_risk, levels = c("<250K", "250K-500K", "500K-750K", ">750K"))

# Créer le graphique à partir des données risk_data avec la palette de couleurs Viridis
plot_ly(risk_data_balance, x = ~risk_intensity_balance/100, y = ~current_balance_risk, 
        color = ~risk_quality, colors = palette_colors, 
        type = "bar", orientation = "h") %>%
  layout(title = "Dormant Risk Intensity by current balance",
         xaxis = list(title = "Risk Intensity (%)", tickformat = ".1%"),
         yaxis = list(title = "Age Group"),
         barmode = "stack", # Utilisation de barmode = "stack" pour empiler les barres
         legend = list(title = "Risk Quality"))
