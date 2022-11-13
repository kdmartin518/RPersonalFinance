#The set of functions that users will interact with the most.

#' Generate an a calendar of transactions and cumulative balances for an arbitrary number of months.
#'
#' @param transaction_sheet A Generic bank account DataFrame.
#' @param start_month_num An object of class "integer". Number of first month in calendar.
#' @param year An object of class "integer". First year of calendar.
#' @param num_months How many months to generate.
#' @return Returns an object of class "DataFrame"
#' @examples
create_balance_sheet <- function(transaction_sheet,from_date,to_date) {
  date <- from_date
  list_of_months <- data.frame('date'=Date(),'amount'=numeric())
  
  # Generate n months of transaction lists.
  # calculate N
  num_months <- month(to_date)-month(from_date) + 1 + 12*(year(to_date)-year(from_date))
  
  for (i in 1:num_months) {
    
    list_of_months <- rbind(list_of_months,
                            aggregate_transactions_by_bankaccount(transaction_sheet,
                                                                  month(date),
                                                                  year(date)))
    date <- date + months(1)
  }
  
  #Filter dates outside parameters.
  balance_sheet <- list_of_months %>% filter(between(date,from_date,to_date))
  
  # calculate cumulative sum column
  balance_sheet <-  balance_sheet %>% 
    group_by(bank_account) %>% 
    mutate(balance = cumsum(amount)) 
  
  return(balance_sheet)
}

draw_balance_sheet <- function(balance_sheet) {
  
  balance_sheet <- mutate(balance_sheet,month_group = as.character(month(date)))
  
  ggplot(data = balance_sheet, aes(x=date)) + 
    geom_bar(aes(y=amount, fill=balance_sheet$bank_account),stat="identity", position="dodge") + 
    geom_line(aes(y=balance, col=balance_sheet$bank_account),stat="identity", show.legend = FALSE)
  #scale_y_continuous(breaks=seq(-2000,2000, by = 250))
  
}


predict_max_overdraft <- function(balance_sheet) {
  
  minimums <- balance_sheet %>% 
    group_by(bank_account) %>% 
    summarise(balance=min(balance)) %>% 
    arrange(bank_account)
  
  minimums <- merge(minimums,balance_sheet,on=c('balance','bank_account'),all=FALSE)
  minimums <- minimums[!duplicated(minimums$bank_account),]
  minimums <- minimums %>% 
    transmute(date,balance,bank_account) %>% 
    filter(balance<0) %>%
    arrange(date,balance,bank_account)
  
  return(minimums)
}