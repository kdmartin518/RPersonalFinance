library(tidyverse)
library(tidyr)
library("lubridate") 
library(ggplot2)

wday_name <- list()
wday_name[["Sunday"]] <- 1
wday_name[["Monday"]] <- 2
wday_name[["Tuesday"]] <- 3
wday_name[["Wednesday"]] <- 4
wday_name[["Thursday"]] <- 5
wday_name[["Friday"]] <- 6
wday_name[["Saturday"]] <- 7

import_bills <- function(csv) {
  
  print("import_bills START:")
  
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
  
  #remove NA day values
  bills <- filter(bills,!is.na(bills$day))
  
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
  
  print("import_bills OUTPUT:")
  print(listof_bankaccounts)
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
  
  print("import_transfers:")
  print(listof_bankaccounts)
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


#' Apply true month dates to bank account.
#'
#' Given a generic bank account DataFrame and a month and year, generate the actual 
#' dates that the bank account's transactions will land on in that month.
#' @param bankaccount An object of class "DataFrame". An individual bank account.
#' @param month_num An object of class "integer". Number of intended month.
#' @param year An object of class "integer". Year.
#' @return Returns an object of class "DataFrame". A DataFrame with the bank account data and real date values. Sorted by date.
#' @examples
#' januaryTransactions <- list_transactions_dates_for_month(genericTransactions,1,2022)
list_transactions_with_dates_for_month <- function(bankaccount,month_num,year) {
  
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

#' Convert and split a DataFrame of weekday transactions into frame with true dates.
#'
#' Given a month, year, and a DataFrame with a list of transactions listed as occurring weekly on every "___day" in a month,
#' Find the true date of each of those weekdays in that month and create a new transaction line with 1/4 of the "monthly_amount".
#' @param weekday_rows An object of class "DataFrame". Contains rows of transaction information where "day" is a weekday like "Monday".
#' @param month_num An object of class "integer". Number of intended month.
#' @param year An object of class "integer". Year.
#' @return Returns an object of class "DataFrame". A DataFrame of transactions with actual dates in the given month.
#' @examples#' 
#' #weekday_rows contains:
#' # a transaction that occurs on Monday with a monthly total amount of $100. 
#' januaryTransactions <- list_transactions_dates_for_month(transactions,1,2022) 
#' #januaryTransactions contains: 
#' # $25 on 01/03/22
#' # $25 on 01/10/22
#' # $25 on 01/17/22
#' # $25 on 01/24/22
#' # $25 on 01/31/22
split_weekdays_into_dates <- function(weekday_rows,month_num,year) {
  
  # Prep DataFrame for output.
  dated_rows <- data.frame(
                  date=ymd(),
                  name=as.character(),
                  amount=as.numeric()
                )
  
  # Loop through each weekday in weekday_rows
  for (i in 1:length(weekday_rows$weekday_name)) {
    
    weekday_name <- weekday_rows$weekday_name[i]
    #Skip if empty.
    if (is.na(weekday_name)) next
    
    # Split weekday into set of real dates with 1/4 the monthly value.
    date <- get_weekdays_in_month(weekday_name,month_num,year)
    name <- weekday_rows$name[i]
    amount <- (weekday_rows$amount[i])/4
    
    # Bind to output
    df <- data.frame(date,name,amount)
    dated_rows <- rbind(dated_rows,df)
    
  }

  return(dated_rows)
  
}

#' Find the actual dates that a given weekday land on in a month.
#'
#' @param weekday_name The name of a given week.
#' @param month_num An object of class "integer". Number of intended month.
#' @param year An object of class "integer". Year.
#' @return Returns an object of class "List". A List with the true dates.
#' @examples
#' Example input: Oct, 2022, Friday
#' Example output: 10/7/22, 10/14/22, 10/21/22, 10/28/22
get_weekdays_in_month <- function(weekday_name,month_num,year) {
  
  # To be honest, I think this is around where I started to invoke some dark magic that I don't understand and don't want to ask too much about.
  
  starting_date <- mdy(paste(month_num,1,year,sep = '-'))
  length <- days_in_month(starting_date)
  
  date <- seq.Date(from = starting_date, to = starting_date+length-1, by = 'days')
  weekday <- wday(date)
  
  df <- data.frame(date, weekday)
  
  df <- filter(df, weekday == wday_name[[weekday_name]])

  return(df$date) 
}

#' Generate a DataFrame with one column, a list of days in a given month from 1-n.
#'
#' @param month_num An object of class "integer". Number of intended month.
#' @param year An object of class "integer". Year.
#' @return Returns an object of class "DataFrame".
#' @examples
get_list_of_days_in_month <- function(month_num,year) {
  
  starting_date <- mdy(paste(month_num,1,year,sep = '-'))
  
  length <- days_in_month(starting_date)
  
  df <- data.frame(
    date = seq.Date(from = starting_date, to = starting_date+length-1, by = 'days')
  )

  return(seq.Date(from = starting_date, to = starting_date+length-1, by = 'days'))
}  

#' Given a generic bank account Data Frame, return a calendar of dates in a specific month with aggregate transaction amounts.
#'
#' @param bankaccount A generic bank account DataFrame.
#' @param month_num An object of class "integer". Number of intended month.
#' @param year An object of class "integer". Year.
#' @return Returns an object of class "DataFrame". 
#' @examples
aggregate_transactions_by_date_in_month <- function(bankaccount,month_num,year) {  
  
  listOfTransactionsWithDates <- list_transactions_with_dates_for_month(bankaccount,month_num,year)

  date <- ymd(paste(year,month_num,1,sep = "-"))
  
  listOfTransactionsWithDates <- listOfTransactionsWithDates %>% 
    group_by(date) %>%
    summarise(amount = sum(amount))
  
  #Now merge onto a calendar with list of transactions and cumulative balance

  month <- data.frame(
                          date=get_list_of_days_in_month(month_num,year),
                          amount=0)

  month = merge(month,listOfTransactionsWithDates, by=c("date","amount"), all=TRUE) 

    return(month)
  
}

#' Generate an a calendar of transactions and cumulative balances for an arbitrary number of months.
#'
#' @param bankaccount A Generic bank account DataFrame.
#' @param start_month_num An object of class "integer". Number of first month in calendar.
#' @param year An object of class "integer". First year of calendar.
#' @param num_months How many months to generate.
#' @return Returns an object of class "DataFrame"
#' @examples
create_balance_sheet <- function(bankaccount,from_date,to_date,starting_balance=0) {
  date <- from_date
  list_of_months <- data.frame('date'=Date(),'amount'=numeric())
  
  # Generate n months of transaction lists.
    # calculate N
  num_months <- month(to_date)-month(from_date) + 1 + 12*(year(to_date)-year(from_date))

  for (i in 1:num_months) {
    
    list_of_months <- rbind(list_of_months,
                            aggregate_transactions_by_date_in_month(bankaccount,
                                                                    month(date),
                                                                    year(date)))
    date <- date + months(1)
  }
  
  #Filter dates outside parameters.
  balance_sheet <- list_of_months %>% filter(between(date,from_date,to_date))
    
  #add custom starting balance
  balance_sheet$amount[1] = starting_balance
  
  # calculate cumulative sum column
  balance_sheet <- balance_sheet %>% mutate(balance = cumsum(amount)) 
    
  
  return(balance_sheet)
}

draw_balance_sheet <- function(balance_sheet) {
  
  balance_sheet <- mutate(balance_sheet,month_group = as.character(month(date)))

  ggplot(data = balance_sheet, aes(x=date)) + 
    geom_bar(aes(y=amount),stat="identity", fill=balance_sheet$month_group, show.legend = FALSE) + 
    geom_line(aes(y=balance),stat="identity", col=balance_sheet$month_group, show.legend = FALSE)
  #scale_y_continuous(breaks=seq(-2000,2000, by = 250))

}
bills <- import_bills('bills.csv')
transfers <- import_transfers('transfers.csv')
merge <- merge_bankaccounts_lists(bills,transfers)

from_date <- mdy('04-01-2022')
to_date <- mdy('05-01-2022')
balance_sheet <- create_balance_sheet(merge[[5]],from_date,to_date)

draw_balance_sheet(balance_sheet)
