library(dplyr)

single_ticker_close_values <- function( ticker, from_year ) {
  URL <- sprintf('http://ichart.finance.yahoo.com/table.csv?s=%s&a=1&b=1&c=%d&g=d&ignore=.csv', ticker, from_year)
  data <- read.csv(URL, stringsAsFactors=FALSE)
  data$Date <- as.Date(data$Date)
  data <- data[,c('Date', 'Close')]

  return(data)
}

rename_column <- function( df, oldname, newname) {
  colnames <- names(df)
  colnames[which(colnames==oldname)] <- newname
  names(df) <- colnames
  
  return(df)
}

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