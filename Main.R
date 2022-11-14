library(tidyverse)
library(tidyr)
library(lubridate) 
library(ggplot2)

source('constants.R')
source('importer_functions.R')
source('helper_functions.R')
source('user_functions.R')

#Import data
bills <- 'data/bills.csv'
transfers <- 'data/transfers.csv'
special_sheet <- import_special('data/special.csv')

transaction_sheet <- import_data(bills,transfers)

print(transaction_sheet)

#Set range of dates to interact with
from_date <- mdy('11-01-2022')
to_date <- mdy('11-30-2022')

#Create an aggregation of transactions per date per bank account.
balance_sheet <- create_balance_sheet(transaction_sheet,from_date,to_date)

#Filter out a subset of bank accounts
alysia <- filter(balance_sheet,bank_account == 'Alysia Credit' | bank_account == 'Alysia Primary' | bank_account == 'Alysia Spending')

#Plot balance and transactions for this sheet.
draw_balance_sheet(alysia)
predict_max_overdraft(balance_sheet)
