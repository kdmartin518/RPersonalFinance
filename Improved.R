library(tidyverse)
library(tidyr)
library("lubridate")   

# I've structures my personal finance planning like so:
# Budget.csv. A spreadsheet of anticipated monthly expenses.Includes these columns:

  # Accountor	      Character   Name of person responsible for this expense
  # Bank Account	  Character   Name of bank account that this expense will be posted to.
  # Category	      Character   Descriptive type for expense.
  # Subcategory	    Character   Secondary descriptive type for expense.
  # Due Day	        Character   Day of month when expense will post. Can be an integer, interpreted as day of month, or a character, interpreted as day of week and will then spread that expense across the month into four transactions. 
  # Name	          Character   Name or description of expense.
  # Monthly Amount	Numeric     Dollar amount of expense
  # Autopay	        Logical     Whether expense has autopay activated.
  # Description     Character   Misc. notes

# Transfers.csv A spreadsheet of antipated transfers between bank accounts. 
  #Its columns are the same as budget.csv EXCEPT "Bank account" is replaced with

  # Bank Account To
  # Bank Account From

  #And "Subcategory" is omitted.
  
# I chose to separate these two sheets because if transfers were to be listed in budget.csv,
  # each transaction would be listed twice: once for the bank account "to" and once for the bank account "from".
  # By using a separate spreadsheet, I can format it differently and include a "to" and "from" column, 
  # which condenses all transfers into a single line.
  
  # PS If you think this system is incomprehensible and bad, send me an email with your suggestion! 
  # I sincerely welcome it. But I will consider the system itself outside the scope of this project,
  # which instead is focused on writing an R script to acommodate its idosyncracies. 

# I would like to rename budgets.csv to bills.csv, and include non-scheduled expenses in another file.

# The first thing to do is to import the CSVs into a set of dataframes, 
  # split on the "Bank Account" (or corresponding To and From) columns.

import_bills <- function(csv) {
  
  #Import CSV
  file <- read.csv(csv, header=TRUE)
  
  #Load CSV into data frame
  bills <- data.frame(
    accountor = file$Accountor,
    bank_account = file$Bank.Account,
    day = as.numeric(file$Due.Day),
    name = file$Name,
    monthly_amount = as.numeric(gsub("\\$|,","",file$Monthly.Amount))
  )
  
  #Fill empty monthly_amount cells with 0.
  bills$monthly_amount[is.na(bills$monthly_amount)] <- 0
  
  #Identify unique bank account names and count them.
  unique_accounts = unique(bills$bank_account)
  num_accounts = length(unique_accounts)
  
  #Split the bills dataframe into a list of dataframes where each df contains data from a single unique bank account.
  listof_bankaccounts <- list()
  for(i in 1:num_accounts) {        
    
    #Get data from bank account
    bank_account_name <- unique_accounts[i]
    bank_account_data <- filter(bills, bank_account == bank_account_name)
    
    #Skip to next bank account if this one has no
    if (nrow(bank_account_data) == 0) next
    
    #Bind the rows we just extracted to a new data frame in the list.
    df1 <- listof_bankaccounts[[bank_account_name]] 
    df2 <- transmute(bank_account_data, accountor = accountor, day = day, name = name, monthly_amount = monthly_amount)
    listof_bankaccounts[[bank_account_name]] <- rbind(df1,df2)          
  }
  
  return(listof_bankaccounts)    
  
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
  
  listof_bankaccounts <- list()
  
  for(i in 1:length(unique_accounts_from)) {
    bank_account_name <- unique_accounts_from[i]
    bank_account_data <- filter(transfers, bank_account_from == bank_account_name)
    if (nrow(bank_account_data) == 0) next
    df1 <- listof_bankaccounts[[bank_account_name]] 
    df2 <- transmute(bank_account_data, accountor = accountor, day = day, name = name, monthly_amount=-1*monthly_amount) #Notice here that monthly amount is multipied *-1.
    listof_bankaccounts[[bank_account_name]] <- rbind(df1,df2)
  }
  
  for(i in 1:length(unique_accounts_to)) {
    bank_account_name <- unique_accounts_to[i]
    bank_account_data <- filter(transfers, bank_account_to == bank_account_name)
    if (nrow(bank_account_data) == 0) next
    df1 <- listof_bankaccounts[[bank_account_name]] 
    df2 <- transmute(bank_account_data, accountor = accountor, day = day, name = name, monthly_amount=monthly_amount)
    listof_bankaccounts[[bank_account_name]] <- rbind(df1,df2)
  }
  
  return(listof_bankaccounts)
  
}

# Now we have imported our CSVs into a common dataframe format.
  # However, since we did this in two different functions, we have two different 
  # sets of same bank accounts: one from bills, one from transfers.
  # We would like to be able to combine them into a singular set of dataframes.

# This function will take two lists of bank account dataframes and merge them into one.
# Note that if list 2 contains a bank account that list 1 does not, that bank account
# will be excluded from the merged result.

merge_bankaccounts_lists <- function(list1,list2) {
  merged_list <- list()
  for (i in 1:length(list1)) {
    bankaccount <- names(list1)[i]
    df1 <- list1[[bankaccount]]
    df2 <- list2[[bankaccount]]
    merged_list[[bankaccount]] <- rbind(df1,df2)           
  }
  return(merged_list)
}

# The day column is not very strict, and this is by design. 
  # You can write in an integer day number or a weekday name, 
  # and later we can calculate a real date for them.

# Convert bank account days to true dates
generate_month_from_bankaccount <- function(bankaccount,month_num,year) {
  
  #Split number days and named weekdays into their own columns.
  bankaccount <-  bankaccount %>% 
                  mutate(
                    weekday = ifelse(grepl("([A-Za-z])", day), day, NA),
                    day = as.integer(day)
                  ) %>% 
                  select(day,weekday,name,monthly_amount) %>%
                  arrange(day)
  
  #Now separate weekday rows and numbered day rows into two data frames.
  weekdays <-   bankaccount %>% 
                filter(!is.na(weekday)) %>% 
                transmute(
                  weekday_names = weekday,
                  name = name,
                  amount = monthly_amount
                )
  month <-   bankaccount %>% filter(is.na(weekday)) %>%
                  transmute(
                    date = ymd(paste(year,month_num,day,sep = " ")),
                    name = name,
                    amount = monthly_amount
                  )                     
  
  #Now convert the weekday rows into numbered_day rows and merge back into the same dataframe.
  if (nrow(weekdays) > 0) {
    weekdays <- split_weekdays_into_dates(weekdays,month_num,year)
    rbind(month,weekdays)
  }
  
  #Sort by date
  arrange(month,date)
  
  return(month)
  
}

split_weekdays_into_dates <- function(weekday_rows,month_num,year) {
  
  dated_rows <- data.frame(
                  date=ymd(),
                  name=as.character(),
                  amount=as.numeric()
                )
  
  for (i in 1:length(weekday_rows$weekday_name)) {
    
    weekday_name <- weekday_rows$weekday_name[i]
    
    if (is.na(weekday_name)) next
    
    date <- get_weekdays_in_month(input_date,weekday_name)
    name <- weekday_rows$name[i]
    amount <- (weekday_rows$amount[i])/4
    df <- data.frame(date,name,amount)
    dated_rows <- rbind(dated_rows,df)
    
  }
  
  return(weekday_rows)
  
}

get_weekdays_in_month <- function(weekday_name,month_num,year) {
  
  #Example input: Oct, 2022, Friday
  #Example output: 10/7/22, 10/14/22, 10/21/22, 10/28/22
  
  starting_date <- mdy(paste(month_num,1,year,sep = '-'))
  length <- days_in_month(starting_date)
  
  date <- seq.Date(from = starting_date, to = starting_date+length-1, by = 'days')
  weekday <- wday(date)
  
  df <- data.frame(date, weekday)
  
  df <- filter(df, weekday == wday_name[[weekday_name]])
  
  return(df$date) #Boy it works but it's starting to get pretty loosey goosey in here.
}

generate_days_in_month <- function(month_num,year) {
  #Given a bank account calendar, generate real date data
  starting_date <- mdy(paste(month_num,1,year,sep = '-'))
  
  length <- days_in_month(starting_date)
  
  df <- data.frame(
    date = seq.Date(from = starting_date, to = starting_date+length-1, by = 'days')
  )
  return(df)
}  

generate_calendar_from_bankaccount <- function(bankaccount,month_num,year) {    
  
  ShortCalendar <- generate_month_from_bankaccount(bankaccount,month_num,year)
  
  date <- mdy(paste(month_num,1,year,sep = '-'))
  
  ShortCalendar <-
    ShortCalendar %>% 
    group_by(date) %>%
    summarise(amount = sum(amount))
  
  #Now merge onto a calendar with list of transactions and cumulative balance
  calendar = merge(
    generate_days_in_month(month_num,year),
    ShortCalendar, by="date", all=TRUE) 
  
  calendar[is.na(calendar)] <- 0 #Replace NA values with 0.
  
  return(calendar)
  
}

generate_seriesof_calendars <- function(bankaccount,start_month_num,start_year,num_months) {
  date <- ymd(paste(start_year,start_month,1,sep = " "))
  lastCalendar <- generate_calendar_from_bankaccount(bankaccount,date)
  looongCalendar <- data.frame()
  looongCalendar <- rbind(looongCalendar,lastCalendar)
  for (i in 2:num_months-1) {
    
    month(date) <- month(date)+1
    
    lastCalendar <- generate_calendar_from_bankaccount(bankaccount,date)
    looongCalendar <- rbind(looongCalendar,lastCalendar)
  }
  return(looongCalendar)
}

get_ending_balance <- function(calendar) {   
  return(calendar$balance[length(calendar$balance)])
}

bills <- import_bills('bills.csv')

convert_bankaccount_to_month(bills[[5]],1,2022)
