source('taxlosslib.R')

do_harvest <- F

capital_gains_tax <- 0.25

tickers <- c('SPY', 'EZU', 'GDX')
intended_weights <- list()
for(t in tickers)
  intended_weights[t] <- 1/length(tickers)
start_value <- 1e6

df <- multiple_ticker_close_values(tickers, 2009)

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
  
  max_weight - min_weight > 0.05
} 

last_tax_year <- ""
do_taxes <- function(date) {
  current_year <- format(date,'%Y')
  
  if(current_year != last_tax_year) {
    last_tax_year <<- current_year
    return(TRUE)
  }
  
  return(FALSE)
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
    history$price    <- c(history$price, price)
   } else if(quantity<0) {
    indices_to_remove <- NULL
    for(idx in length(history$quantity):1) {
      transaction_q <- history$quantity[idx]
      transaction_p <- history$price[idx]
      
      if(transaction_q > -quantity) {
        history$quantity[idx] <- transaction_q + quantity
        capital_gain_loss <<- capital_gain_loss + (-quantity)*(price-transaction_p) # Global assignment
        break
      } else {
        indices_to_remove <- c(indices_to_remove, idx)
        quantity <- quantity + transaction_q #remember quantity < 0; bring closer to 0
        capital_gain_loss <<- capital_gain_loss + transaction_q*(price-transaction_p) # Global assignment      
      }     
    }
    
    if(!is.null(indices_to_remove)) {
      history$quantity <- history$quantity[-indices_to_remove]
      history$price <- history$price[-indices_to_remove]
    }
  }
  
  if(min(history$quantity) <= 0)
    stop('Oh, dear. Negative quantity in buy history.')
  
  buy_history[[ticker]] <<- history #Global assignment
}

harvest <- function(current_prices) {
  for(t in tickers) {
    history <- buy_history[[t]]  
    price <- current_prices[[t]]
    
    buys_to_remove <- NULL
    quantity_to_buy <- 0
    for(idx in length(history$quantity):1) {
      bought_price <- history$price[idx]
      bought_quantity <- history$quantity[idx]
      
      if(bought_price > price) {
        buys_to_remove <- c(buys_to_remove, idx)
        quantity_to_buy <- quantity_to_buy + bought_quantity
        
        # Accumulate capital losses
        capital_gain_loss <<- capital_gain_loss + bought_quantity*(price-bought_price) # Global
      } else break
    }
    
    # If we discovered anything to sell out, sell and rebuy
    if(!is.null(buys_to_remove)) {
      history$quantity <- history$quantity[-buys_to_remove]
      history$price <- history$price[-buys_to_remove]
      
      buy_history[[t]] <<- history # Global
      transact_in_ticker(t, quantity_to_buy, price) # Now rebuy the sold out quantity
    }
  }
}

values <- NULL
untaxed_values <- NULL
gain_loss_history <- NULL
taxes_paid <- NULL
for(index in 1:dim(df)[1]) {
  row <- df[index,]
  current_date <- row$Date
  current_quantities <- calculate_quantities(buy_history)
  tax_time <- do_taxes(current_date) #Remembers year so can only be called once per date
#   print(sprintf('Gain/loss on %s: %f', current_date, capital_gain_loss))
  gain_loss_history <- c(gain_loss_history, capital_gain_loss)
  
  if(do_reweighting(current_date, current_quantities) || tax_time) {
    print(sprintf('Reweighing on %s', current_date))
    
    current_quantities <- calculate_quantities(buy_history)
    current_weights    <- calculate_weights(current_quantities, current_date)
    total_value        <- calculate_total_value(current_quantities, row)
    
    if(do_harvest) {
     harvest(row)
    }  
    
    # Now we reweight
    for(t in tickers) {
      intended_quantity <- total_value * intended_weights[[t]] / row[[t]]
      to_buy_quantity <- intended_quantity - current_quantities[[t]]
      
#       print(sprintf('to buy quantity on %s: %f', t, to_buy_quantity))
      transact_in_ticker(t, to_buy_quantity, current_price) 
    }
  }
  
  if(tax_time){
    print(sprintf('capital gain/loss: %f', capital_gain_loss))
   if(capital_gain_loss > 0) {
     tax_to_pay <- capital_gains_tax * capital_gain_loss
     taxes_paid <- c(taxes_paid, tax_to_pay)
     print(sprintf('Paid tax of %f on %s', tax_to_pay, current_date))
     
     capital_gain_loss <- 0
     
     current_quantities <- calculate_quantities(buy_history)
     current_value <- calculate_total_value(current_quantities, row)
     if(tax_to_pay > current_value)
       stop( "Not enough money to pay tax")
     
     tax_per_ticker <- tax_to_pay / length(tickers)
     
     for(t in tickers) {
       current_price <- row[[t]]
       transact_in_ticker(t, -tax_per_ticker/current_price, current_price)
     }
   } 
  }
  
  # Calculate investment value
  quantities <- calculate_quantities(buy_history)
  tax_to_pay <- max(capital_gain_loss,0)*capital_gains_tax
  total_value <- calculate_total_value(quantities, row) - tax_to_pay
  values <- c(values, total_value)
  untaxed_values <- c(untaxed_values, calculate_total_value(quantities, row))
}

plot(df$Date, values, type='l')

