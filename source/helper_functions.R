#' Given a set of transaction sheets ordered by bank account, aggregate each bank account's transactions by date.
#'
#' @param transaction_sheet An object of class "data.frame". 
#' @param month_num An object of class "integer". Number of intended month.
#' @param year An object of class "integer". Year.
#' @return Returns an object of class "data.frame". 
aggregate_transactions_by_bankaccount <- function(transaction_sheet,month_num,year) {  
  aggregated_sheet <- data.frame()
  
  unique_bankaccounts <- unique(transaction_sheet$bank_account)
  num_bankaccounts <- length(unique_bankaccounts)

  #Iterate through each bank account in transaction_sheet
  for (i in 1:num_bankaccounts) {
    this_bankaccount <- unique_bankaccounts[i]
    bankaccount_transactions <- transaction_sheet %>% 
                                filter(bank_account == this_bankaccount) %>%
                                transmute(day,name,monthly_amount)
    
    # Get an individual transaction sheet for this account and month.
    bankaccount_transactions <- aggregate_transactions_by_date_in_month(bankaccount_transactions,month_num,year)
    bankaccount_transactions <- mutate(bankaccount_transactions,bank_account = this_bankaccount)
    
    # Check for any special transactions for this account and month.
    # If there are any, merge them into the output.
    special_transactions <- special_sheet %>%
      filter(bank_account==this_bankaccount) %>%
      transmute(bank_account,date,amount)
    if (nrow(special_transactions) > 0) {
      bankaccount_transactions <- merge(bankaccount_transactions,special_transactions,on = "date",all=TRUE)
      bankaccount_transactions <- bankaccount_transactions %>%
                                  group_by(bank_account, date) %>%
                                  summarise(amount = sum(amount))
    }
    
    #Bind rows for this account and month into the output.
    aggregated_sheet <- rbind(aggregated_sheet,bankaccount_transactions)
  }
  
  return(aggregated_sheet)
}

#' Given a transaction sheet for a single bank account, aggregate transactions by day of month.
#'
#' @param transaction_sheet An object of class "data.frame".
#' @param month_num An object of class "integer". Number of intended month.
#' @param year An object of class "integer". Year.
#' @return Returns an object of class "data.frame". 
aggregate_transactions_by_date_in_month <- function(transaction_sheet,month_num,year) {  
  date <- ymd(paste(year,month_num,1,sep = "-"))
  
  #Convert this bank account's generic days into dates for this month.
  listOfTransactionsWithDates <- list_transactions_with_dates_for_month(transaction_sheet,month_num,year)
  listOfTransactionsWithDates <- listOfTransactionsWithDates %>% 
    group_by(date) %>%
    summarise(amount = sum(amount))
  
  #Set up a frame with number of days in this month.
  month <- data.frame(date=get_list_of_days_in_month(month_num,year))
  
  #Merge aggregate transactions onto month.
  month = merge(month,listOfTransactionsWithDates, by=c("date"),all.x=TRUE) 
  
  #Turn na values to 0.
  month[is.na(month)]=0
  
  return(month)
}

#' Apply true month dates to bank account.
#'
#' Given a generic bank account DataFrame and a month and year, generate the actual 
#' dates that the bank account's transactions will land on in that month.
#' @param transaction_sheet An object of class "data.frame". An individual bank account.
#' @param month_num An object of class "integer". Number of intended month.
#' @param year An object of class "integer". Year.
#' @return Returns an object of class "DataFrame". A DataFrame with the bank account data and real date values. Sorted by date.
#' @examples
#' januaryTransactions <- list_transactions_dates_for_month(genericTransactions,1,2022)
list_transactions_with_dates_for_month <- function(transaction_sheet,month_num,year) {
  
  #Split number days and named weekdays into their own columns.
  transaction_sheet <-  transaction_sheet %>% 
    mutate(
      weekday = ifelse(grepl("([A-Za-z])", day), day, NA),
      day = as.integer(day)
    ) %>% 
    select(day,weekday,name,monthly_amount) %>%
    arrange(day)
  
  #Now separate weekday rows and numbered day rows into two data frames.
  weekdays <-   transaction_sheet %>% 
    filter(!is.na(weekday)) %>% 
    transmute(
      weekday_names = weekday,
      name = name,
      amount = monthly_amount
    )
  month <-   transaction_sheet %>% filter(is.na(weekday)) %>%
    transmute(
      date = ymd(paste(year,month_num,day,sep = " ")),
      name = name,
      amount = monthly_amount
    )                     
  
  #Now convert the weekday rows into numbered_day rows and merge back into the same dataframe.
  if (nrow(weekdays) > 0) {
    weekdays <- split_weekdays_into_dates(weekdays,month_num,year)
    month <- rbind(month,weekdays)
  }
  
  #Sort by date
  month <- arrange(month,date)
  
  return(month)
}

#' Convert and split a DataFrame of weekday transactions into frame with true dates.
#'
#' Given a month, year, and a DataFrame with a list of transactions listed as occurring weekly on every "___day" in a month,
#' Find the true date of each of those weekdays in that month and create a new transaction line with 1/4 of the "monthly_amount".
#' @param weekday_rows An object of class "data.frame". Contains rows of transaction information where "day" is a weekday like "Monday".
#' @param month_num An object of class "integer". Number of intended month.
#' @param year An object of class "integer". Year.
#' @return Returns an object of class "data.frame". A DataFrame of transactions with actual dates in the given month.
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
#' @param weekday_name An object of class "character". The name of a given week.
#' @param month_num An object of class "integer". Number of intended month.
#' @param year An object of class "integer". Year.
#' @return Returns an object of class "List". A List with the true dates.

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
#' @return Returns an object of class "data.frame".
get_list_of_days_in_month <- function(month_num,year) {
  
  starting_date <- mdy(paste(month_num,1,year,sep = '-'))
  
  length <- days_in_month(starting_date)
  
  df <- data.frame(
    date = seq.Date(from = starting_date, to = starting_date+length-1, by = 'days')
  )
  
  return(seq.Date(from = starting_date, to = starting_date+length-1, by = 'days'))
}  

