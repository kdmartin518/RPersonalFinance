library(tidyverse)
library(tidyr)
library("lubridate")     

# I don't think that there's a library that lets you input a weekday by string and have it return a number, so here's this.
wday_name <- list()
wday_name[["Sunday"]] <- 1
wday_name[["Monday"]] <- 2
wday_name[["Tuesday"]] <- 3
wday_name[["Wednesday"]] <- 4
wday_name[["Thursday"]] <- 5
wday_name[["Friday"]] <- 6
wday_name[["Saturday"]] <- 7

###  SECTION 1: Import CSV Data into a data frame.
# CSVs rows list expected transactions per date per bank account. 
# This is notated slightly differently between bduget.csv and transfer.csv.
# The following functions import the CSVs into a common data frame format:
# Each bank account is split up into a separate data frame.
# All data frames are contained in a list. 

import_budget <- function(csv) {
  
  #Import CSV
  file <- read.csv(csv, header=TRUE)
  
  #Load CSV into data frame
  budget <- data.frame(
    accountor = file$Accountor,
    bank_account = file$Bank.Account,
    day = as.numeric(file$Due.Day),
    name = file$Name,
    monthly_amount = as.numeric(gsub("\\$|,","",file$Monthly.Amount))
  )
  
  budget$monthly_amount[is.na(budget$monthly_amount)] <- 0
  
  #Split data by accountors and bank accounts.
  unique_accounts = unique(budget$bank_account)
  num_accounts = length(unique_accounts)
  
  listof_bankaccounts <- list()
  
  for(i in 1:num_accounts) {        
    
    bank_account_name <- unique_accounts[i]
    rows <- filter(budget, bank_account == bank_account_name)
    if (nrow(rows) == 0) next
    
    df1 <- listof_bankaccounts[[bank_account_name]] 
    df2 <- transmute(rows, accountor = accountor, day = day, name = name, monthly_amount = monthly_amount)
    listof_bankaccounts[[bank_account_name]] <- rbind(df1,df2)          
  }
  
  return(listof_bankaccounts)    
  
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
  
  #Split data by accountors and bank accounts.    
  unique_accounts_from = unique(transfers$bank_account_from)
  unique_accounts_to = unique(transfers$bank_account_to)    
  
  listof_bankaccounts <- list()
  
  for(i in 1:length(unique_accounts_from)) {
    bank_account_name <- unique_accounts_from[i]
    rows <- filter(transfers, bank_account_from == bank_account_name)
    if (nrow(rows) == 0) next
    df1 <- listof_bankaccounts[[bank_account_name]] 
    df2 <- transmute(rows, accountor = accountor, day = day, name = name, monthly_amount=-1*monthly_amount)
    listof_bankaccounts[[bank_account_name]] <- rbind(df1,df2)
    
  }
  
  for(i in 1:length(unique_accounts_to)) {
    bank_account_name <- unique_accounts_to[i]
    rows <- filter(transfers, bank_account_to == bank_account_name)
    if (nrow(rows) == 0) next
    df1 <- listof_bankaccounts[[bank_account_name]] 
    df2 <- transmute(rows, accountor = accountor, day = day, name = name, monthly_amount=monthly_amount)
    listof_bankaccounts[[bank_account_name]] <- rbind(df1,df2)
  }
  
  return(listof_bankaccounts)
  
}

merge_budget_with_transfers <- function(budget,transfer) {
  merged_list <- list()
  for (i in 1:length(budget)) {
    
    bankaccount <- names(budget)[i]
    df1 <- budget[[bankaccount]]
    #print(df1)
    df2 <- transfer[[bankaccount]]
    #print(df2)
    merged_list[[bankaccount]] <- rbind(df1,df2)           
    
  }
  return(merged_list)
}


### SECTION 2: The next several functions handle converting the bank account format into a nice calendar format. 
# Where the bank account data frames from above list each transaction along with amount and day, 
# a calendar aggregates each day's worth of transactions into a sum

# There is a quirk in my budget format that I'll try to describe as linearly as possible here:
# Some transactions may have day = "Friday" or "monday", etc instead of 2, 5, 17#
# This implies a transaction which occurs on a weekly basis instead of monthly. 
# Note also that in the CSV the column is labeled "monthly amount," which is still true for these weekly transactions.
# Interpret them this way: For a transaction like "Groceries, $400, Fridays", plan to spend $100 every Friday.
# Explicitly stated: A Weekday transaction is repeated weekly with an amount of (Monthly Amount)/4. 
#                                                *Even if there are more or less than four Fridays in a month.*

create_month <- function(input_date) {
  #Given a bank account calendar, generate real date data
  starting_date <- input_date
  
  length <- days_in_month(starting_date)
  
  df <- data.frame(
    date = seq.Date(from = starting_date, to = starting_date+length-1, by = 'days')
  )
  return(df)
}  

calculate_weekdays_in_month <- function(input_date,weekday_name) {
  
  #Example input: Oct, 2022, Friday
  #Example output: 10/7/22, 10/14/22, 10/21/22, 10/28/22
  
  starting_date <- input_date
  length <- days_in_month(starting_date)
  
  date <- seq.Date(from = starting_date, to = starting_date+length-1, by = 'days')
  weekday <- wday(date)
  
  df <- data.frame(date, weekday)
  
  df <- filter(df, weekday == wday_name[[weekday_name]])
  
  return(df$date) #Boy it works but it's starting to get pretty loosey goosey in here.
}

split_weekdays_into_monthly_transactions <- function(listof_weekdaytransactions,input_date) {
  weekday_rows <- data.frame(date=ymd(),name=as.character(),amount=as.numeric())
  for (i in 1:length(listof_weekdaytransactions$weekday_name)) {
    weekday_name <- listof_weekdaytransactions$weekday_name[i]
    
    if (is.na(weekday_name)) next
    date <- calculate_weekdays_in_month(input_date,weekday_name)
    name <- listof_weekdaytransactions$name[i]
    amount <- (listof_weekdaytransactions$amount[i])/4
    df <- data.frame(date,name,amount)
    weekday_rows <- rbind(weekday_rows,df)
    
  }
  
  return(weekday_rows)
  
}

convert_bankaccount_to_month <- function(bankaccount,input_date) {
  
  data <- bankaccount %>% 
    mutate(
      weekday = ifelse(grepl("([A-Za-z])", day), day, NA),
      day = as.integer(day)
    ) %>% 
    select(day,weekday,name,monthly_amount) %>%
    arrange(day)
  
  weekdays <- data %>% filter(!is.na(weekday)) %>% transmute(weekday_names = weekday,name = name,amount = monthly_amount)
  
  month_data <- data %>% filter(is.na(weekday)) %>%
    transmute(
      date = ymd(paste(year(input_date),month(input_date),day,sep = " ")),
      name = name,
      amount = monthly_amount)                     
  
  if (nrow(weekdays) > 0) {
    weekdays <- split_weekdays_into_monthly_transactions(weekdays,input_date)
    rbind(month_data,weekdays)
  }
  print(month_data)
  arrange(month_data,date)
  
  
  return(month_data)
}

#Converts a single bank account budget listing into a long calendar.
generate_calendar_from_bankaccount <- function(bankaccount,input_date) {    
  
  ShortCalendar <- convert_bankaccount_to_month(bankaccount,input_date)
  
  ShortCalendar <-
    ShortCalendar %>% 
    group_by(date) %>%
    summarise(amount = sum(amount))
  
  #Now merge onto a calendar with list of transactions and cumulative balance
  calendar = merge(
    create_month(input_date),
    ShortCalendar, by="date", all=TRUE) #the all=TRUE is vital here, otherwise it would skip all days not included i nthe short calendar, which defeats the poiunt.
  
  calendar[is.na(calendar)] <- 0 #Replace NA values with 0.
  
  return(calendar)
  
}


# Accepts a bank account and generates a a calendar of n months long.
generate_seriesof_calendars <- function(bankaccount,start_month,start_year,num_months) {
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

### SECTION 3: Viz

draw_transactions_v_balance <- function(data,date_earliest,date_latest) {
  
  data <- data %>% filter(between(date,date_earliest,date_latest))
  
  ggplot(data = data, aes(x=date)) + 
    #geom_bar(aes(y=amount),stat="identity") + 
    geom_line(aes(y=balance),stat="identity") #+
  #scale_x_continuous(breaks=c(0,7,14,21,28),minor_breaks=seq(0, 31, by = 1)) +
  #scale_y_continuous(breaks=seq(-2000,2000, by = 250))
}

budget <- import_budget("budget.csv")
transfers <- import_transfers("transfer.csv")
listof_bankaccounts <- merge_budget_with_transfers(budget,transfers)

###
data <- listof_bankaccounts[["Alysia Primary"]]
quarter <- generate_seriesof_calendars(data,"October",2022,3)
quarter <- quarter %>% 
  filter(between(date,mdy("10-17-2022"),mdy("01-01-2024")))           
quarter$amount[1]=quarter$amount[1]+191 
quarter <- quarter %>%
  mutate(balance = cumsum(amount))
print(quarter)
draw_transactions_v_balance(quarter,mdy("1-15-2022"),mdy("12-15-2023"))

###
