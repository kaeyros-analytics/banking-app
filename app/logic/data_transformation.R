# C:/Kaeyros/compte dormant/bank/app/logic/data_transformation.R

box:: use(jsonlite[fromJSON], lubridate[month], dplyr[mutate,if_else])

# paths to JSON files
chemin_accounts <- "C:/Kaeyros/compte dormant/data/accounts.json"
chemin_agios <- "C:/Kaeyros/compte dormant/data/agios.json"
chemin_transactions <- "C:/Kaeyros/compte dormant/data/transactions.json"
# Reading of json bases and motification of columns format 
account <- fromJSON(chemin_accounts)
account <- account|>
  mutate(dateofbirth = as.Date(dateofbirth, format = "%Y-%m-%d"),
         closing_date = as.Date(closing_date, format = "%Y-%m-%d"),
         creation_date = as.Date(creation_date, format = "%Y-%m-%d"),
         account_id = as.character(account_id),
         status = if_else(is.na(closing_date),"Active","Inactive") # variable of interest 
  )


agios <- fromJSON(chemin_agios)
colnames(agios)[2] <- "Year_agios"
agios$Year_agios <-  as.Date(agios$Year_agios, format = "%Y-%m-%d")
agios$month_agios <- lubridate::month(agios$Year_agios)
agios$account_id <- as.character(agios$account_id)


transactions <- fromJSON(chemin_transactions)
transactions <- transactions |>
  mutate(account_id = as.character(account_id),
         transaction_id = as.character(transaction_id),
         transaction_date = as.Date(transaction_date, format = "%Y-%m-%d"))
