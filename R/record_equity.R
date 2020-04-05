#' Write financing entries in the ledger.
#' @param date        Date. date of the emission.
#' @param quantity    Integer. Quantity of stock emitted.
#' @param price       Double. Market value of the stock emitted.
#' @param par         Deouble. Book value of the stock emitted.
#' @param origin      Character. Account where the stock is accounted for.
#' @param destination Character. Account where the counterpart is accounted for.
#' @return A tibble of journal entries.
#' @importFrom dplyr case_when
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom FinancialMath amort.period
#' @export


record_equity <- function(date = Sys.Date(),
                          quantity = 100,
                          price = 100,
                          par = 100,
                          origin = 31100,
                          destination = 10030){
  
  
  type_fin <- dplyr::case_when(
    origin < 31200 & origin >= 31100 & destination == 10030 ~ "common",
    origin < 31300 & origin >= 31200 & destination == 10030 ~ "noncumulative",
    origin < 31400 & origin >= 31300 & destination == 10030 ~ "cumulative",
    origin < 31500 & origin >= 31400 & destination == 10030 ~ "treasuryout",
    origin == 10030 & destination < 31500 & destination >= 31400 ~ "treasuryin",
    TRUE ~ "dividend"
  )
  
  
  entries <- list()
  
  
  if (type_fin == "common"){
    
    label <- paste0(quantity, " common shares issued, par value of ", par, ", for a price of ", price)
    
    cash_amount <- quantity * price
    stock_amount <- par * quantity
    paidin_amount <- (price - par) * quantity
    if (paidin_amount < 0) {
      paidin_debit <- abs(paidin_amount)
      paidin_credit <- NA
    } else {
      paidin_debit <- NA
      paidin_credit <- abs(paidin_amount)
    }
    
    entries[[1]] <- tibble::tibble(
      date = rep(date,3),
      label = c(rep(label,3)),
      account = c(10030,38100,31100),
      debit = c(cash_amount,paidin_debit,NA),
      credit = c(NA,paidin_credit,stock_amount)
    )
        
    
  } else if(type_fin == "noncumulative"){
    
    label <- paste0(quantity, " noncumulative preferred shares issued, par value of ", par, ", for a price of ", price)
    
    cash_amount <- quantity * price
    stock_amount <- par * quantity
    paidin_amount <- (price - par) * quantity
    if (paidin_amount < 0) {
      paidin_debit <- abs(paidin_amount)
      paidin_credit <- NA
    } else {
      paidin_debit <- NA
      paidin_credit <- abs(paidin_amount)
    }
    
    entries[[1]] <- tibble::tibble(
      date = rep(date,3),
      label = c(rep(label,3)),
      account = c(10030,38200,31200),
      debit = c(cash_amount,paidin_debit,NA),
      credit = c(NA,paidin_credit,stock_amount)
    )
    
    
  } else if(type_fin == "cumulative"){
    
    label <- paste0(quantity, " cumulative preferred shares issued, par value of ", par, ", for a price of ", price)
    
    cash_amount <- quantity * price
    stock_amount <- par * quantity
    paidin_amount <- (price - par) * quantity
    if (paidin_amount < 0) {
      paidin_debit <- abs(paidin_amount)
      paidin_credit <- NA
    } else {
      paidin_debit <- NA
      paidin_credit <- abs(paidin_amount)
    }
    
    entries[[1]] <- tibble::tibble(
      date = rep(date,3),
      label = c(rep(label,3)),
      account = c(10030,38300,31300),
      debit = c(cash_amount,paidin_debit,NA),
      credit = c(NA,paidin_credit,stock_amount)
    )
    
    
  } else if(type_fin == "treasuryout"){
    
    label <- paste0(quantity, " treasury shares issued, par value of ", par, ", for a price of ", price)
    
    amount <- quantity * price
    
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = c(rep(label,2)),
      account = c(10030,31400),
      debit = c(amount,NA),
      credit = c(NA,amount)
    )
    
  } else if(type_fin == "treasuryin"){
    
    label <- paste0(quantity, " treasury shares bought, par value of ", par, ", for a price of ", price)
    
    cash_amount <- quantity * price
    stock_amount <- par * quantity
    paidin_amount <- (price - par) * quantity
    if (paidin_amount < 0) {
      paidin_debit <- NA
      paidin_credit <- abs(paidin_amount)
    } else {
      paidin_debit <- NA
      paidin_credit <- abs(paidin_amount)
    }
    
    entries[[1]] <- tibble::tibble(
      date = rep(date,3),
      label = c(rep(label,3)),
      account = c(31400,38400,10030),
      debit = c(stock_amount,paidin_debit,NA),
      credit = c(NA,paidin_credit,cash_amount)
    )
    
    
  } else {
    
    dividend <- quantity * price
    date_decision <- date - 15
    label_de <- paste0("record the dividend of ", dividend, " decided on ", date_decision)
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = c(rep(label,2)),
      account = c(origin,destination),
      debit = c(dividend,NA),
      credit = c(NA,dividend)
    )
    
    label_pay <- paste0("payment of the dividend of ", dividend, " recorded on ", date)
    date_pay <- date + 60
    entries[[2]] <- tibble::tibble(
      date = rep(date_pay,2),
      label = c(rep(label_pay,2)),
      account = c(destination,10030),
      debit = c(dividend,NA),
      credit = c(NA,dividend)
    )
    
  }
  
  entries <- dplyr::bind_rows(entries)
  
  return(entries)
}
