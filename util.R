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
## 1. get root dir ----
root <- getwd()
#path_data <- paste(root, "/", "data", sep="")


# paths to JSON files
chemin_accounts <- "C:/Kaeyros/compte dormant/data1/accounts.json"
chemin_agios <- "C:/Kaeyros/compte dormant/data1/agios.json"
chemin_transactions <- "C:/Kaeyros/compte dormant/data1/transactions.json"
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
                            labels = c("<30","30-40","40-50", "50-60", ">=60")),
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

write_xlsx(account,"C:/Kaeyros/compte dormant/data/account.xlsx")
write_xlsx(agios,"C:/Kaeyros/compte dormant/data/agios.xlsx")
write_xlsx(transactions,"C:/Kaeyros/compte dormant/data/transactions.xlsx")






