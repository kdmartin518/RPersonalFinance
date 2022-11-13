library(tidyverse)
library(tidyr)
library("lubridate") 
library(ggplot2)

source('Constants.R')
source('importer_functions.R')
source('user_functions.R')

#Import data
bills <- import_bills('data/bills.csv')
transfers <- import_transfers('data/transfers.csv')
#special <- import_special('data/special.csv') TODO
merge <- merge_transaction_sheets(bills,transfers)

#Set range of dates to interact with
from_date <- mdy('04-01-2022')
to_date <- mdy('05-01-2022')

#Create an aggregation of transactions per date per bank account.
balance_sheet <- create_balance_sheet(merge,from_date,to_date)

#Filter out a subset of bank accounts
alysia <- filter(balance_sheet,bank_account == 'Alysia Credit' | bank_account == 'Alysia Primary' | bank_account == 'Alysia Spending')

#Plot balance and transactions for this sheet.
draw_balance_sheet(alysia)