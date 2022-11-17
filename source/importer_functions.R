# Functions to help import budget sheets into the program.

import_bills <- function(csv) {
  
  #Import CSV
  file <- read.csv(csv, header=TRUE)
  
  #Load CSV into data frame
  transaction_sheet <- data.frame(
    accountor = file$Accountor,
    bank_account = file$Bank.Account,
    day = as.numeric(file$Due.Day),
    name = file$Name,
    monthly_amount = as.numeric(gsub("\\$|,","",file$Monthly.Amount))
  )
  
  #remove NA day values
  transaction_sheet <- filter(transaction_sheet,!is.na(transaction_sheet$day))
  
  #Fill empty monthly_amount cells with 0.
  transaction_sheet$monthly_amount[is.na(transaction_sheet$monthly_amount)] <- 0
  
  transaction_sheet <- transaction_sheet %>% 
    transmute(bank_account = bank_account, accountor = accountor, day = day, name = name, monthly_amount = monthly_amount) %>%
    arrange(bank_account)
  
  return(transaction_sheet)    
  
}

import_transfers <- function(csv) {
  
  #Import CSV
  file <- read.csv(csv, header=TRUE)
  
  #Load CSV into data frame
  transfers <- data.frame(
    accountor = file$Accountor,
    bank_account_to = file$Bank.Account.To,
    bank_account_from = file$Bank.Account.From,
    day = file$Due.Day,
    name = file$Name,
    monthly_amount = as.numeric(gsub("\\$|,","",file$Monthly.Amount))
  )
  transfers$monthly_amount[is.na(transfers$monthly_amount)] <- 0
  
  #Split data by bank accounts.    
  # Here's the main difference: Since we have two bank account columns, we basically just do the same thing as before, but twice.
  # Transactions are recorded as a negative value to bank accounts in the From column, and then as a positive value to bank accounts in the To column. 
  unique_accounts_from = unique(transfers$bank_account_from)
  unique_accounts_to = unique(transfers$bank_account_to)    
  
  transaction_sheet <- data.frame(bank_account = character(), 
                                  accountor = character(), 
                                  day = character(), 
                                  name = character(), 
                                  monthly_amount = numeric()
  )
  
  for(i in 1:length(unique_accounts_from)) {
    bank_account_name <- unique_accounts_from[i]
    bank_account_data <- filter(transfers, bank_account_from == bank_account_name)
    if (nrow(bank_account_data) == 0) next
    transaction_sheet <- rbind(transaction_sheet,
                               transmute(bank_account_data, bank_account = bank_account_name, accountor = accountor, day = day, name = name, monthly_amount=-1*monthly_amount)) #Notice here that monthly amount is multipied *-1.)
  }
  
  for(i in 1:length(unique_accounts_to)) {
    bank_account_name <- unique_accounts_to[i]
    bank_account_data <- filter(transfers, bank_account_to == bank_account_name)
    if (nrow(bank_account_data) == 0) next
    transaction_sheet <- rbind(transaction_sheet,
                               transmute(bank_account_data, bank_account = bank_account_name, accountor = accountor, day = day, name = name, monthly_amount=monthly_amount))
  }
  
  transaction_sheet <- arrange(transaction_sheet,bank_account)
  
  return(transaction_sheet)
  
}

import_special <- function(csv) {
  
  #Import CSV
  file <- read.csv(csv, header=TRUE)
  
  #Load CSV into data frame
  transaction_sheet <- data.frame(
    bank_account = file$Bank.Account,
    date = as.Date(mdy(file$Date)),
    name = file$Name,
    amount = as.numeric(gsub("\\$|,","",file$Amount))
  )

  #remove NA day values
  transaction_sheet <- filter(transaction_sheet,!is.na(transaction_sheet$date))
  
  #Fill empty monthly_amount cells with 0.
  transaction_sheet$monthly_amount[is.na(transaction_sheet$amount)] <- 0
  
  transaction_sheet <- transaction_sheet %>% 
    transmute(bank_account,date,name,amount) %>%
    arrange(bank_account)
  
  return(transaction_sheet)    
  
}

merge_transaction_sheets <- function(sheet1,sheet2) {
  merged_sheet <- rbind(sheet1,sheet2)
  merged_sheet <- arrange(merged_sheet,bank_account)
  return(merged_sheet)
}

import_data <- function(bills,transfers) {
  
  bills <- import_bills(bills)
  transfers <- import_transfers(transfers)
  merge <- merge_transaction_sheets(bills,transfers)
  
  return(merge)
}

