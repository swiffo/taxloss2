source('taxlosslib.R')

capital_gains_tax <- 0.25
buy_history <- NULL # Simply ensure the variable is in this scope


tickers <- c('SPY', 'EZU', 'GDX')
df <- multiple_ticker_close_values(tickers, 2009) # Can be moved inside loop

#' set_up_buyhistory
#' 
#' initializes buy_history (global scope) with the purchases needed to reach the target weights.
set_up_buyhistory <- function(target_weights, prices, value=1e6) {
  hist <- list()
  tickers <- names(target_weights)
  for(t in tickers) {
    price <- prices[[t]]
    quantity <- start_value / price * target_weights[[t]]
    hist[[t]] <- list(quantity=quantity, price=price)
  }
  
  buy_history <<- hist # Global assignment
}
capital_gain_loss <- 0 

#' do_rebalancing
#' 
#' Returns TRUE/FALSE depending on whether the portfolio should be rebalanced.
do_rebalancing <- function(weights) {
  max_weight <- NULL
  min_weight <- NULL
  maxmin <- function(w) {
    max_weight <<- max(max_weight, w)
    min_weight <<- min(min_weight, w)
  }
  lapply(weights, maxmin)
  
  max_weight - min_weight > 0.05
} 

last_tax_year <- format(df[1,'Date'],'%Y')
do_taxes <- function(date) {
  current_year <- format(date,'%Y')
  
  if(current_year != last_tax_year) {
    last_tax_year <<- current_year
    return(TRUE)
  }
  
  return(FALSE)
}

#' calculate_total_value
#' 
#' Returns total portfolio value based on input quantities and prices.
calculate_total_value <- function(quantities, prices) { 
  total <- 0
  for(t in tickers) 
    total <- total + quantities[[t]]*prices[[t]]
  
  total
}

calculate_termination_value <- function(buy_history, prices, capital_gain_loss) {
  quantities <- calculate_quantities(buy_history)
  total_value <- calculate_total_value(quantities, prices)
  
  PNL <- 0
  
  for(t in tickers) {
    hist <- buy_history[[t]]
    current_price <- prices[[t]]
    for(idx in 1:length(hist$quantity)) {
      PNL <- PNL + hist$quantity[idx] * (current_price - hist$price[idx])
    }
  }
  
  tax_to_pay <- max(capital_gain_loss + PNL,0)*capital_gains_tax
  
  total_value - tax_to_pay
}

#' transact_in_ticker
#' 
#' Buys or sells in given ticker the stated quantity at the stated price.
#' NOTE: This modifies 'capital_gain_loss' and 'buy_history' in the wider world.
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
  print(sprintf('cap gain loss after transaction: %f', capital_gain_loss))
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

run_simulation <- function(
 tickers, # vector of tickers
 do_harvest=TRUE # whether to do tax-loss harvesting
) {  
  # Equi-weighted portfolio
  intended_weights <- sapply(tickers, function(x) 1/length(tickers))
  start_value <- 1e6
  
  set_up_buyhistory(intended_weights, df[1,])
  
  values <- NULL # Vector of values assuming the portfolio was terminated (and thus subject to tax)
  pretax_values <- NULL # Vector of current value of portfolio (pre-tax)
  gain_loss_history <- NULL # Vector of accumulated capital gains/losses
  taxes_paid <- NULL # Vector of the taxes paid (variable length)
  for(index in 1:dim(df)[1]) {
    row <- df[index,]
    current_date <- row$Date
    current_quantities <- calculate_quantities(buy_history)
    current_weights <- calculate_weights(current_quantities, row)
    
    tax_time <- do_taxes(current_date) #Remembers year so can only be called once per date
    gain_loss_history <- c(gain_loss_history, capital_gain_loss)
    
    # Rebalance portfolio on sunny days
    if(do_rebalancing(current_weights) || tax_time) {
      print(sprintf('Rebalancing on %s', current_date))
      total_value <- calculate_total_value(current_quantities, row)
      
      if(do_harvest) {
        harvest(row)
      }  
      
      # Now we rebalance
      for(t in tickers) {
        intended_quantity <- total_value * intended_weights[[t]] / row[[t]]
        to_buy_quantity <- intended_quantity - current_quantities[[t]]
        current_price <- row[[t]]
        
        transact_in_ticker(t, to_buy_quantity, current_price) 
      }
      print(sprintf('cap gain/loss after rebal on %s: %f', current_date, capital_gain_loss))
    }
    
    if(tax_time){
      print(sprintf('Taxable on %s: %f', current_date, capital_gain_loss))
      if(capital_gain_loss > 0) {
        tax_to_pay <- capital_gains_tax * capital_gain_loss
        taxes_paid <- c(taxes_paid, tax_to_pay)
        print(sprintf('Paid tax of %f on %s', tax_to_pay, current_date))
        
        capital_gain_loss <<- 0
        
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
    total_value <- calculate_termination_value(buy_history, row, capital_gain_loss)
    values <- c(values, total_value)
    pretax_values <- c(pretax_values, calculate_total_value(quantities, row))
  }
  
  list(
    dates=df$Date,
    values=values,
    gain_loss_history=gain_loss_history,
    taxes_paid=taxes_paid
  )
}

output <- run_simulation(tickers, do_harvest=FALSE)
plot(output$dates, output$values, type='l')

