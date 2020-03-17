#' Write entries in the ledger related to purchases.
#' @param date        Date. date of the sale.
#' @param object      Character. Name of the product or service sold.
#' @param quantity    Integer. Volume sold.
#' @param price       Double. Base price of the product or service.
#' @param discount    Double. Percentage of price reduction earned if payment within terms.
#' @param vat         Double. Percentage of price to compute value added taxes.
#' @param dpo         Integer. Day-Payable-Outstading or customer credit.
#' @param risk        Double. Probability of not paying on time.
#' @param lifetime    Integer. For how many periods does the right hold.
#' @param nature      Character. Account where the asset is "stored".
#' @param destination Character. Account where the asset depreciation is accumulated.
#' @return A tibble of journal entries.
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @importFrom lubridate as_date
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate days_in_month
#' @importFrom lubridate day
#' @importFrom dplyr bind_rows
#' @importFrom stats runif
#' @export


record_purchases <- function(date = Sys.Date(),
                             object = "units",
                             quantity = 100,
                             price = 10,
                             discount = 0.05,
                             vat = 0.2,
                             dpo = 45,
                             risk = 0.1,
                             lifetime = 1,
                             nature = 21000,
                             destination = 13300){
  
  acc_cash <- 10100
  acc_vatx <- 24100
  acc_adj <- 58000
  
  
  type_purchase <- dplyr::case_when(
    nature < 15000 ~ "prepaid",
    TRUE ~ "due"
  )
  
  entries <- list()
  
  
  if (type_purchase == "prepaid"){
    
    date_recept <- date
    lubridate::day(date_recept) <- 1
    lubridate::month(date_recept) <- lubridate::month(date_recept) + lifetime
    lubridate::day(date_recept) <- lubridate::days_in_month(date_recept)
    
    label_pay <- paste0("payment in advance for ", object)
    label_use <- paste0("usage of ", object)
    
    va_tax <- price * vat
    payment <- price + va_tax
    
    entries[[1]] <- tibble::tibble(
      date = c(rep(date,3),rep(date_recept, 2)),
      label = c(rep(label_pay,3),rep(label_use, 2)),
      account = c(nature, acc_vatx, acc_cash, destination, nature),
      debit = c(price,va_tax,NA,price,NA),
      credit = c(NA,NA,payment,NA,price)
    )
    
    
  } else {
    
    label_purch <- paste0("purchase of ", quantity, " ", object)
    purchase <- quantity * price
    vat_amount <- vat * purchase
    payable <- purchase + vat_amount
    entries[[1]] <- tibble::tibble(
      date = rep(date,3),
      label = rep(label_purch,3),
      account = c(destination,acc_vatx,nature),
      debit = c(purchase,vat_amount,NA),
      credit = c(NA,NA,payable)
    )
    
    situation <- runif(1)
    situation <- dplyr::case_when(
      situation < risk ~ "pay late",
      TRUE ~ "pay on time"
    )
    
    if (situation == "pay late"){
      
      lateness <- floor(15*runif(1))
      label_cashout <- paste0("payment for the ", label_purch, " on ", date)
      entries[[2]] <- tibble::tibble(
        date = rep(date+dpo+lateness,2),
        label = rep(label_cashout,2),
        account = c(nature, acc_cash),
        debit = c(payable,NA),
        credit = c(NA,payable)
      )
      
    } else {
      
      earliness <- floor(5*runif(1))
      label_cashout <- paste0("payment for the ", label_purch, " on ", date)
      cancel_purch <- discount * purchase
      cancel_vat <- vat_amount * discount
      payment <- payable - cancel_purch - cancel_vat 
      entries[[2]] <- tibble::tibble(
        date = rep(date+dpo-earliness,4),
        label = rep(label_cashout,4),
        account = c(nature,acc_cash,acc_adj,acc_vatx),
        debit = c(payable,NA,NA,NA),
        credit = c(NA,payment,cancel_purch,cancel_vat)
      )
      
    }
    
  }
  
  entries <- dplyr::bind_rows(entries)
  
  return(entries)
}