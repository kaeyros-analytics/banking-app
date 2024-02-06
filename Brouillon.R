
df_account <- account

# convert the values from variable dateofbirth to date format
df_account$dateofbirth1 <-  as.Date(df_account$dateofbirth, format = "%Y-%m-%d")
str(df_account)

class(df_account$dateofbirth)
typeof(df_account$dateofbirth)
library(lubridate)

#convert character to date format
df_account$dateofbirth1 <-  lubridate::ymd(df_account$dateofbirth)

df_agios <- agios
str(df_agios)

# assigning the second column name to a new name 
colnames(agios)[2] <- "Year_agios"

df_transactions <- transactions
str(df_transactions)
df_transactions <- df_transactions %>%
  mutate(account_id = as.character(account_id),
         transaction_id = as.character(transaction_id),
         transaction_date = as.Date(transaction_date, format = "%Y-%m-%d"))

colnames(account)

dftest <- account %>%
  select(creation_date,closing_date,status)

table(account$closing_date, useNA = "ifany") 

table(account$status, useNA = "ifany") 

max(account$creation_date)

length(account$account_id)
length(unique(account$account_id))

dfname <- account %>%
  select(account_id,first_name,last_name,citizenship)

df_join   

test_last <- test %>%
  group_by(account_id) %>%
  mutate(last_transaction_date = max(transaction_date))


totalCustomers_df <- reactive({
      account %>%
        filter(transaction_date >= input$dateRangeInput[1],
               transaction_date <= input$dateRangeInput[2],
               status == input$statusFilter)
  })
  



agios[-year_agios]



# Calculer le nombre de clients par statut et par agence
df_status_by_agency <- account %>%
  select(account_id,location, gender) %>%
  distinct(account_id,location, .keep_all = TRUE) %>%
  group_by(location, gender) %>%
  summarise(customer_count = n()) %>% 
  filter(location=="Bamenda")

# Créer le graphique Highchart
highchart() %>%
  hc_chart(type = "bar") %>%
  hc_xAxis(categories = unique(lapply(df_status_by_agency$location, function(x) list(x)))) %>%
  hc_add_series(
    name = "Men", 
    data = df_status_by_agency %>%
      filter(gender == "Men") %>%
      pull(customer_count)
  ) %>%
  hc_add_series(
    name = "Women", 
    data = df_status_by_agency %>%
      filter(gender == "Women") %>%
      pull(customer_count)
  ) %>%
  hc_title(text = "Customer Status by city") %>%
  hc_yAxis(title = list(text = "Number of Customers")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_plotOptions(grouping = FALSE)



