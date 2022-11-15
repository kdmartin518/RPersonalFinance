library(tidyverse)
library(tidyr)
library(lubridate) 
library(ggplot2)

source('constants.R')
source('importer_functions.R')
source('helper_functions.R')
source('user_functions.R')




# 
# 

# 
# #Filter out a subset of bank accounts
# alysia <- filter(balance_sheet,bank_account == 'Alysia Credit' | bank_account == 'Alysia Primary' | bank_account == 'Alysia Spending')
# 
# #Plot balance and transactions for this sheet.
# draw_balance_sheet(alysia)
# predict_max_overdraft(balance_sheet)
# 
# print(transaction_sheet)
# 
# #TODO: Create dummy data, write R Markdown notebook with demonstration, complete function documentation, implement compare against real bank transaction data.
