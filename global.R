# Charger le package
library(jsonlite)
library(shiny)
library(tidyverse)
library(readxl)
library(dplyr)
library(shinydashboard)
library(DiagrammeR) # is required to plot a `data.tree::Node`object.
library(data.tree)
library(shinythemes)
library(shinycssloaders)
require(lubridate)
library(DBI)
library(readr)
library(xml2)
library(XML)
library(DT)
library(partykit)
library(stringr)
library(lubridate)
library(tools)
library(reactlog)
library(writexl)
library(lubridate)
library(highcharter)
library(leaflet)
library(rhandsontable)
library(formattable)
library(shinyWidgets)
## 1. get root dir ----
root <- getwd()
#path_data <- paste(root, "/", "data", sep="")


# paths to JSON files
chemin_accounts <- "C:/Kaeyros/banking-app/data1/accounts.json"
chemin_agios <- "C:/Kaeyros/banking-app/data1/agios.json"
chemin_transactions <- "C:/Kaeyros/banking-app/data1/transactions.json"
# Reading of json bases and motification of columns format 
account <- fromJSON(chemin_accounts)
account <- account %>% 
  mutate(dateofbirth = as.Date(dateofbirth, format = "%Y-%m-%d"),
         closing_date = as.Date(closing_date, format = "%Y-%m-%d"),
         creation_date = as.Date(creation_date, format = "%Y-%m-%d"),
         account_id = as.character(account_id),
         age = as.integer(floor(as.numeric(difftime(Sys.Date(), dateofbirth, units = "days")) / 365.25)),
         gender = recode(gender, "Homme" = "Men", "Femme" = "Women")) 

# Filtrer les âges pour qu'ils se situent dans la plage de 8 à 90 ans
#account$age <- pmax(pmin(account$age, 90), 8)
# Ajout de la colonne classe d'age
account <- account %>%
  mutate(age_interval = cut(age, breaks = c(-Inf, 30, 40, 50, 60, Inf), 
                            labels = c("<30","30-39","40-49", "50-59", ">=60")),
         monthly_revenue_class = cut(average_monthly_revenue, breaks = c(-Inf,250000,500000,750000,Inf),
                                     labels = c("<250K", "250K-500K", "500K-750K", ">750K"))
  )
# Créer une nouvelle colonne avec les valeurs modifiées
account$client_id <- paste0("client_", seq_along(account$account_id) - 1)


# separation des coordonnées de lonfitutute , latitude
latitude = c()
longitude = c()
for (coord in  account$geo_location){
  latitude<- c(latitude,coord[1])
  longitude <- c(longitude,coord[2])
}
account$latitude <- latitude
account$longitude <- longitude
# Supprimer la colonne geo_location car nous l'avons maintenant séparée en latitude et longitude
account$geo_location <- NULL  # Supprimer la colonne geo_location

agios <- fromJSON(chemin_agios)
colnames(agios)[2] <- "year_agios"
agios$year_agios <-  as.Date(agios$year_agios, format = "%Y-%m-%d")
agios$month_agios <- lubridate::month(agios$year_agios)
agios$account_id <- as.character(agios$account_id)
agios <- agios %>% 
  mutate(
    balance_after_agios_class = cut(balance_after_agios,breaks = c(-Inf,0,1000000,5000000,Inf),
                                    labels = c("<0M","0M-1M","1M-5M",">5M"))
  )

transactions <- fromJSON(chemin_transactions)
transactions <- transactions %>%
  mutate(account_id = as.character(account_id),
         transaction_id = as.character(transaction_id),
         transaction_date = as.Date(transaction_date, format = "%Y-%m-%d"))

# Export to excel file

write_xlsx(account,"C:/Kaeyros/banking-app/data1/account.xlsx")
write_xlsx(agios,"C:/Kaeyros/banking-app/data1/agios.xlsx")
write_xlsx(transactions,"C:/Kaeyros/banking-app/data1/transactions.xlsx")

################## MODELISATION #########################

# 1. je commence par isoler tout les clients actifs depuis 2014 jusqu'au 30/06/2023
## ceci constitue ma periode d'analyse
# 2. ensuite je crèe six database ou j'évalue sur chaque mois l'etat de ses clients s'il sont devenue Actif ou inactif entre le 01/07/2023 et le 31/12/2023
## ceci constitue ma periode d'observation
# 3. j'effectif six modèle de regression logistique pour Obtenir le PD sur six mois

population <- account %>% 
  inner_join(transactions, by = "account_id",relationship = "many-to-many") %>% 
  group_by(account_id) %>%
  # Calcul de la date de la dernière transaction pour chaque compte
  mutate(last_transaction_date = max(transaction_date, na.rm = TRUE)) 

# "Inactive"==0, "Active"==1
data1 <- (population %>% 
            filter(transaction_date < as.Date("2018-01-31")) %>% 
            mutate(status = if_else((as.Date("2018-01-31") - last_transaction_date) > 120, 0, 1),
                   gender = factor(gender),
                   occupation = factor(occupation),
                   location = factor(location)) %>% 
            distinct(account_id,status, .keep_all=TRUE) %>% 
            select(account_id,age_interval,gender,occupation,location,monthly_revenue_class,
                   number_of_children,status,last_name, first_name, age) %>% 
            filter(location != "Douala") %>% 
            arrange(status))[1:20,] 

data2 <- (population %>% 
            filter(transaction_date< as.Date("2018-10-31")) %>% 
            mutate(status = if_else((as.Date("2018-10-31") - last_transaction_date) > 90, 0, 1),
                   gender = factor(gender),
                   occupation = factor(occupation),
                   location = factor(location)) %>% 
            distinct(account_id,status, .keep_all=TRUE) %>% 
            select(account_id,age_interval,gender,occupation,location,monthly_revenue_class,
                   number_of_children,status,last_name, first_name, age) %>% 
            filter(location != "Douala") %>% 
            arrange(status))[1:13,] 

data3 <- (population %>% 
            filter(transaction_date< as.Date("2018-11-30")) %>% 
            mutate(status = if_else((as.Date("2018-11-30") - last_transaction_date) > 90, 0, 1),
                   gender = factor(gender),
                   occupation = factor(occupation),
                   location = factor(location)) %>% 
            distinct(account_id,status, .keep_all=TRUE) %>% 
            select(account_id,age_interval,gender,occupation,location,monthly_revenue_class,
                   number_of_children,status,last_name, first_name, age) %>% 
            filter(location != "Douala")%>% 
            arrange(status))[1:16,] 

data4 <- ((population) %>% 
            filter(transaction_date< as.Date("2018-12-31")) %>% 
            mutate(status = if_else((as.Date("2018-12-31") - last_transaction_date) > 90, 0, 1),
                   gender = factor(gender),
                   occupation = factor(occupation),
                   location = factor(location)) %>% 
            distinct(account_id,status, .keep_all=TRUE) %>% 
            select(account_id,age_interval,gender,occupation,location,monthly_revenue_class,
                   number_of_children,status,last_name, first_name, age) %>% 
            filter(location != "Douala") %>% 
            arrange(status))[1:20,] 

data5 <- (population %>% 
            filter(transaction_date< as.Date("2019-01-31")) %>% 
            mutate(status = if_else((as.Date("2019-01-31") - last_transaction_date) > 90, 0, 1),
                   gender = factor(gender),
                   occupation = factor(occupation),
                   location = factor(location)) %>% 
            distinct(account_id,status, .keep_all=TRUE) %>% 
            select(account_id,age_interval,gender,occupation,location,monthly_revenue_class,
                   number_of_children,status,last_name, first_name, age) %>% 
            filter(location != "Douala") %>% 
            arrange(status))[1:22,] 

data6 <- (population %>% 
            filter(transaction_date< as.Date("2019-02-28")) %>% 
            mutate(status = if_else((as.Date("2019-02-28") - last_transaction_date) > 90, 0, 1),
                   gender = factor(gender),
                   occupation = factor(occupation),
                   location = factor(location)) %>% 
            distinct(account_id,status, .keep_all=TRUE) %>% 
            select(account_id,age_interval,gender,occupation,location,monthly_revenue_class,
                   number_of_children,status,last_name, first_name, age) %>% 
            filter(location != "Douala") %>% 
            arrange(status))[1:25,] 


##### Model 
log_model1 <- glm(status ~ age_interval+gender+ occupation+location + monthly_revenue_class + number_of_children,
                  data = data1[, !colnames(data1) %in% c("account_id")][1:7], family = "gaussian")
log_model2 <- glm(status ~ age_interval+gender+ occupation+location + monthly_revenue_class + number_of_children,
                  data = data1[, !colnames(data2) %in% c("account_id")][1:7], family = "gaussian")
log_model3 <- glm(status ~ age_interval+gender+ occupation+location + monthly_revenue_class + number_of_children,
                  data = data1[, !colnames(data3) %in% c("account_id")][1:7], family = "gaussian")
log_model4 <- glm(status ~ age_interval+gender+ occupation+location + monthly_revenue_class + number_of_children,
                  data = data1[, !colnames(data4) %in% c("account_id")][1:7], family = "gaussian")
log_model5 <- glm(status ~ age_interval+gender+ occupation+location + monthly_revenue_class + number_of_children,
                  data = data5[, !colnames(data1) %in% c("account_id")][1:7], family = "gaussian")
log_model6 <- glm(status ~ age_interval+gender+ occupation+location + monthly_revenue_class + number_of_children,
                  data = data6[, !colnames(data1) %in% c("account_id")][1:7], family = "gaussian")

predict_status <- function(model, new_data) {
  # Prédire les probabilités des classes
  predictions <- predict(model, newdata = new_data, type = "response")
  predicted_classes <- ifelse(predictions > 0.5, "Active", "Inactive")
  
  return(predicted_classes)
}

# Extraire les coefficients du modèle
coefficients <- coef(log_model1)[-1]  # exclure l'intercept

# Créer un dataframe pour les coefficients et les noms de variable
coefficients_df <- data.frame(
  Feature = names(coefficients),
  Coefficient = coefficients
)

# Trier les coefficients par valeur absolue
coefficients_df <- coefficients_df[order(abs(coefficients_df$Coefficient), decreasing = TRUE), ]

# Créer un graphique à barres pour visualiser les coefficients
ggplot(coefficients_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(x = "Feature", y = "Coefficient", title = "Feature Importance dans le modèle de régression logistique") +
  theme_minimal()

predict_data <- anti_join(account, data6, by = "account_id") %>% 
  select(age_interval,gender,occupation,location,monthly_revenue_class,number_of_children,account_id,last_name, first_name, age) %>% 
  filter(location != "Douala")


predict_data <- predict_data %>%
  mutate(Client_id = paste0("client ", 1:n()))


table_predict_data <- predict_data %>% 
  select(Client_id,first_name,last_name,age,gender,occupation) %>% 
  mutate(
    PD_month1=round(1-1/(1 + exp(-predict(log_model1, newdata = predict_data[1:6], type = "response"))),2),
    PD_month2=round(1-1/(1 + exp(-predict(log_model2, newdata = predict_data[1:6], type = "response"))),2),
    PD_month3=round(1-1/(1 + exp(-predict(log_model3, newdata = predict_data[1:6], type = "response"))),2),
    PD_month4=round(1-1/(1 + exp(-predict(log_model4, newdata = predict_data[1:6], type = "response"))),2),
    PD_month5=round(1-1/(1 + exp(-predict(log_model5, newdata = predict_data[1:6], type = "response"))),2),
    PD_month6=round(1-1/(1 + exp(-predict(log_model6, newdata = predict_data[1:6], type = "response"))),2),
  )



