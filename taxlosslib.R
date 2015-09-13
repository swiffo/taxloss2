library(dplyr)

#' single_ticker_close_values
#'
#' Returns data frame with columns 'Date' and 'Close' for input ticker.
#' Input: from_year should be a string, e.g., '2005'
single_ticker_close_values <- function(ticker, from_year) {
  URL <- sprintf('http://ichart.finance.yahoo.com/table.csv?s=%s&a=1&b=1&c=%d&g=d&ignore=.csv', ticker, from_year)
  data <- read.csv(URL, stringsAsFactors=FALSE)
  data$Date <- as.Date(data$Date)
  data <- data[,c('Date', 'Close')]

  return(data)
}

#' rename_column
#'
#' Renames column in given data frame from oldname to newname. Returns data frame.
rename_column <- function( df, oldname, newname) {
  colnames <- names(df)
  colnames[which(colnames==oldname)] <- newname
  names(df) <- colnames
  
  return(df)
}

#' multiple_ticker_close_values
#'
#' Takes string vector of tickers and returns data frame with columns 'Date' and one for each ticker (named as ticker).
#' Only dates for which there are close values for all tickers are kept in the output.
#' Output data frame is sorted by date (ascending).
multiple_ticker_close_values <- function(tickers, from_year) {
  first_ticker <- tickers[1]
  data <- single_ticker_close_values(first_ticker,from_year)
  data <- rename_column(data, 'Close', first_ticker)
  
  tickers <- tickers[-1]
  for(t in tickers) {
    d <- single_ticker_close_values(t,from_year)
    d <- rename_column(d, 'Close', t)
    data <- inner_join(data, d, by='Date')
  }
  
  data <- arrange(data, Date)
  
  return(data)
}

#' calculate_quantities
#'
#' Takes buy_history, a list of ticker -> 2-element list of price/quantity (same length) and returns
#' a list of ticker -> quantity.
calculate_quantities <- function(buy_history) {
  lapply(buy_history, function(x) sum(x$quantity))
}

#' calculate_weights
#'
#' Takes list of ticker -> quantity, list/data frame row of ticker -> price
#' and returns list of ticker -> weight in portfolio.
calculate_weights <- function(quantities, prices) { 
  weights <- list()
  total <- 0
  
  for(t in tickers) {
    weights[t] <- quantities[[t]] * prices[[t]]
    total <- total + weights[[t]]
  }
  
  lapply(weights, function(v) v/total)  
}