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

# This function is almost identical to import_bills. I'll comment where they diverge.
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

#This CSV contains special values on specific dates. Can be used, for example, to record starting balances for bank accounts.
# Since this outputs a different format that the other two importers IE including a real date, it will need to be merged later on into balance sheet.
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

# Now we have imported our CSVs into a common dataframe format.
# However, since we did this in two different functions, we have two different 
# sets of same bank accounts: one from bills, one from transfers.
# We would like to be able to combine them into a singular set of dataframes.

# This function will take two lists of bank account dataframes and merge them into one.
# Note that if list 2 contains a bank account that list 1 does not, that bank account
# will be excluded from the merged result.
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
