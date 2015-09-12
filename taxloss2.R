source('taxlosslib.R')

capital_gains_tax <- 0.25

tickers <- c('SPY', 'GDX', 'EEM', 'EWJ')
intended_weights <- list()
for(t in tickers)
  intended_weights[t] <- 1/length(tickers)
start_value <- 1e6

df <- multiple_ticker_close_values(tickers, 2010)

first_row <- df[1,]

buy_history <- list()
for(t in tickers) {
  price <- df[1,t]
  quantity <- start_value / price * intended_weights[[t]]
  
  buy_history[[t]] <- list(quantity=quantity, price=price)
}

calculate_quantities <- function(buy_history) {
  lapply(buy_history, function(x) sum(x$quantity))
}

current_year <- format(first_row$Date, '%Y')

capital_gain_loss <- 0 

calculate_weights <- function(quantities, Date) { 
  prices <- df[df$Date==Date,]
  weights <- list()
  total <- 0
  
  for(t in tickers) {
    weights[t] <- quantities[[t]] * prices[t]
    total <- total+weights[[t]]
  }
  
  lapply(weights, function(v) v/total)  
}



do_reweighting <- function(date,quantities) {
  weights <- calculate_weights(quantities, date)
  
  max_weight <- NULL
  min_weight <- NULL
  maxmin <- function(w) {
    max_weight <<- max(max_weight, w)
    min_weight <<- min(min_weight, w)
  }
  lapply(weights, maxmin)
  
  max_weight - min_weight > 0.04
} 

do_taxes <- function() {
  format(current_date, '%m%d')=='1231'
}

calculate_total_value <- function(quantities, prices) { 
  total <- 0
  for(t in tickers) 
    total <- total + quantities[[t]]*prices[[t]]
  
  total
}

transact_in_ticker <- function(ticker, quantity, price) {
  history <- buy_history[[ticker]]
  
  if(quantity>0) {
    history$quantity <- c(history$quantity, quantity)
    history$price <- c(history$price, price)
  } else if(quantity<0) {
    indices_to_remove <- NULL
    for(idx in length(history$quantity):1) {
      q <- history$quantity[idx]
      if(q > -quantity) {
        history$quantity[idx] <- history$quantity[idx] + quantity
        capital_gain_loss <<- capital_gain_loss - quantity*(price-history$price[idx]) # Global assignment
        quantity <- 0
        break
      } else {
        indices_to_remove <- c(indices_to_remove, idx)
        capital_gain_loss <<- capital_gain_loss - quantity*(price-history$price[idx]) # Global assignment
        quantity <- quantity + q
      }
    }
    
    if(!is.null(indices_to_remove)) {
      history$quantity <- history$quantity[-indices_to_remove]
      history$price <- history$price[-indices_to_remove]
    }
  }
  
  buy_history[[ticker]] <<- history #Global assignment
}

values <- NULL
for(index in 1:dim(df)[1]) {
  row <- df[index,]
  current_date <- row$Date
  current_quantities <- calculate_quantities(buy_history)
  
  if(do_reweighting(current_date, current_quantities) || do_taxes()) {
    current_quantities <- calculate_quantities(buy_history)
    current_weights <- calculate_weights(current_quantities, current_date)
    total_value <- calculate_total_value(quantities, row)
    
    for(t in tickers) {
      # Take losses that can be taken
      current_price <- row[[t]]
      history <- buy_history[[t]]
      
      indices_to_remove <- NULL
      for(idx in length(history$quantity):1) {
        buy_price <- history$price[idx]
        to_rebuy <- 0
        if(buy_price >= current_price) {
          q <- history$quantity[idx]
          to_rebuy <- to_rebuy + q*current_price
          indices_to_remove <- c(indices_to_remove, idx)
          capital_gain_loss <- capital_gain_loss + q*(current_price-buy_price)
        } else break
      }
      
      if(to_rebuy > 0) {
        history$quantity <- c(history$quantity[-indices_to_remove], to_rebuy/current_price)
        history$price <- c(history$price[-indices_to_remove], current_price)
      }
      
      # Now we reweight
      intended_quantity <- total_value * intended_weights[[t]] / row[[t]]
      to_buy_quantity <- intended_quantity - current_quantities[[t]]
      
      transact_in_ticker(t, to_buy_quantity, current_price)
    }
  }
  
  if(do_taxes()){
   if(capital_gain_loss > 0) {
     tax_to_pay <- capital_gains_tax * capital_gain_loss
     capital_gain_loss <- 0
     
     if(tax_to_pay > current_value)
       stop( "Not enough money to pay tax")
     
     tax_per_ticker <- tax_to_pay / length(tickers)
     
     for(t in tickers) {
       current_price <- row[[t]]
       transact_in_ticker(t, tax_per_ticker/current_price, current_price)
     }
   } 
  }
  
  # Calculate investment value
  quantities <- calculate_quantities(buy_history)
  total_value <- calculate_total_value(quantities, row) - max(capital_gain_loss,0)*capital_gains_tax
  values <- c(values, total_value)
}

