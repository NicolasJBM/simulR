#' Write entries in the ledger related to sales.
#' @param date             Date. date of the sale.
#' @param object            Character. Name of the product or service sold.
#' @param quantity          Integer. Volume sold.
#' @param price             Double. Base price of the product or service.
#' @param discount          Double. Percentage of price reduction earned if payment within terms.
#' @param vat               Double. Percentage of price to compute value added taxes.
#' @param dso               Integer. Day-Sales-Outstading or customer credit.
#' @param unit_cost         Double. Cost of the product or services sold.
#' @param sales_commission  Double. Percentage of price paid to sales people as commission (end of month).
#' @param labor_tax         Double. Percentage of labor taxes applied to the commissions.
#' @param dco               Integer. Delay before paying commissions.
#' @param account_increment Integer. Last two digits of an account number indicating the destination.
#' @param situation         Character. NULL or specify one of the following situation: "default", "sales return", "pay very late" or "pay late" 
#' @param risk              Double. Number between 0 and 1 indicating the risk on the sale, affecting the probability of different situations.
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



record_sales <- function(date = Sys.Date(),
                         object = "units",
                         quantity = 100,
                         price = 10,
                         discount = 0.05,
                         vat = 0.2,
                         dso = 30,
                         unit_cost = 5,
                         sales_commission = 0.1,
                         labor_tax = 0.5,
                         dco = 15,
                         account_increment = 0,
                         situation = NULL,
                         risk = 0.1){
  
  entries <- list()
  
  
  acc_cash <- 10100
  acc_rece <- 12000
  acc_dbtf <- 12500
  acc_inve <- 13100 + account_increment
  
  acc_comm <- 22200
  acc_vatx <- 24100
  acc_cctx <- 24200
  
  acc_reve <- 40000 + account_increment
  acc_rtrn <- 49000 + account_increment
  acc_badd <- 49100 + account_increment
  acc_disc <- 49500 + account_increment
  acc_cogs <- 50100 + account_increment
  acc_sell <- 61000 + account_increment
  acc_disp <- 71000
  
  
  label_sale <- paste0("sale of ", quantity, " ", object)
  revenue <- quantity * price
  vat_amount <- vat * revenue
  receivable <- revenue + vat_amount
  entries[[1]] <- tibble::tibble(
    date = rep(date,3),
    label = rep(label_sale,3),
    account = c(acc_rece,acc_reve,acc_vatx),
    debit = c(receivable,NA,NA),
    credit = c(NA,revenue, vat_amount)
  )
  
  
  label_inventory <- paste0("sold ", quantity, " ", object)
  out_inventory <- quantity * unit_cost
  entries[[2]] <- tibble::tibble(
    date = rep(date,2),
    label = rep(label_inventory,2),
    account = c(acc_cogs,acc_inve),
    debit = c(out_inventory,NA),
    credit = c(NA,out_inventory)
  )
  
  
  label_commission <- paste0("commission on the sale of ", quantity, " ", object)
  commission <- sales_commission * revenue
  commission_tax <- commission * labor_tax
  selling_expense <- commission + commission_tax
  entries[[3]] <- tibble::tibble(
    date = rep(date,3),
    label = rep(label_commission,3),
    account = c(acc_sell,acc_comm,acc_cctx),
    debit = c(selling_expense,NA,NA),
    credit = c(NA,commission,commission_tax)
  )
  
  label_paycom <- paste0("pay commission on the ", label_sale, " on ", date)
  
  date_com <- date+dco
  lubridate::day(date_com) <- 1
  lubridate::day(date_com) <- lubridate::days_in_month(date_com)
  entries[[4]] <- tibble::tibble(
    date = rep(date_com,2),
    label = rep(label_paycom,2),
    account = c(acc_comm,acc_cash),
    debit = c(commission,NA),
    credit = c(NA,commission)
  )
  
  
  ####################################################################################
  
  
  if (is.null(situation)){
    situation <- runif(1)
    situation <- dplyr::case_when(
      situation < risk / 4 ~ "default",
      situation < risk / 2 ~ "sales return",
      situation < risk  ~ "pay very late",
      situation < risk * 2 ~ "pay late",
      TRUE ~ "pay on time"
    )
  }
  
  
  if (situation == "default"){
    
    label_doubt <- paste0("delay on payment for the ", label_sale, " on ", date)
    entries[[5]] <- tibble::tibble(
      date = rep(date+dso+15,2),
      label = rep(label_doubt,2),
      account = c(acc_dbtf,acc_rece),
      debit = c(receivable,NA),
      credit = c(NA,receivable)
    )
    label_writoff <- paste0("write-off the ", label_sale, " on ", date)
    entries[[6]] <- tibble::tibble(
      date = rep(date+dso+60,3),
      label = rep(label_writoff,3),
      account = c(acc_badd,acc_vatx,acc_dbtf),
      debit = c(revenue,vat_amount,NA),
      credit = c(NA,NA,receivable)
    )
    
  } else if (situation == "sales return"){
    
    date_return <- date + ceiling(20*runif(1))
    quantity_returned <- ceiling(quantity * sample(seq(from = 0.10, to = 0.70, by = 0.1),1))
    cancel_rev <- quantity_returned * price
    cancel_vat <- cancel_rev * vat
    cancel_receivable <- cancel_rev + cancel_vat
    new_revenue <- revenue - cancel_rev
    new_vat_amount <- vat_amount - cancel_vat
    new_receivable <- new_revenue + new_vat_amount
    label_return <- paste0("accepted return from the sale of ", label_sale, " on ", date)
    entries[[5]] <- tibble::tibble(
      date = rep(date_return,3),
      label = rep(label_return,3),
      account = c(acc_rtrn,acc_vatx,acc_rece),
      debit = c(cancel_rev,cancel_vat,NA),
      credit = c(NA,NA,cancel_receivable)
    )
    label_back <- paste0("return of ", quantity_returned, " units on the ", label_sale, " on ", date)
    back_inventory <- quantity_returned * unit_cost
    entries[[6]] <- tibble::tibble(
      date = rep(date_return,4),
      label = rep(label_return,4),
      account = c(acc_inve,acc_cogs,acc_disp,acc_inve),
      debit = c(back_inventory,NA,back_inventory,NA),
      credit = c(NA,back_inventory,NA,back_inventory)
    )
    earliness <- floor(5*runif(1))
    label_cashin <- paste0("received payment for the ", label_sale, " on ", date)
    cancel_new_rev <- discount * new_revenue
    cancel_new_vat <- new_vat_amount * discount
    new_receipt <- new_receivable - cancel_new_rev - cancel_new_vat 
    entries[[7]] <- tibble::tibble(
      date = rep(date+dso-earliness,4),
      label = rep(label_cashin,4),
      account = c(acc_cash,acc_disc,acc_vatx,acc_rece),
      debit = c(new_receipt, cancel_new_rev, cancel_new_vat,NA),
      credit = c(NA,NA,NA,new_receivable)
    )
    
  } else if (situation == "pay very late"){
    
    label_doubt <- paste0("delay on payment for the ", label_sale, " on ", date)
    entries[[5]] <- tibble::tibble(
      date = rep(date+dso+15,2),
      label = rep(label_doubt,2),
      account = c(acc_dbtf,acc_rece),
      debit = c(receivable,NA),
      credit = c(NA,receivable)
    )
    lateness <- 30 + floor(10*runif(1))
    label_cashin <- paste0("received payment for the ", label_sale, " on ", date)
    entries[[6]] <- tibble::tibble(
      date = rep(date+dso+lateness,2),
      label = rep(label_cashin,2),
      account = c(acc_cash,acc_dbtf),
      debit = c(receivable,NA),
      credit = c(NA,receivable)
    )
    
  } else if (situation == "pay late"){
    
    lateness <- floor(15*runif(1))
    label_cashin <- paste0("received payment for the ", label_sale, " on ", date)
    entries[[5]] <- tibble::tibble(
      date = rep(date+dso+lateness,2),
      label = rep(label_cashin,2),
      account = c(acc_cash,acc_rece),
      debit = c(receivable,NA),
      credit = c(NA,receivable)
    )
    
  } else {
    
    earliness <- floor(5*runif(1))
    label_cashin <- paste0("received payment for the ", label_sale, " on ", date)
    cancel_rev <- discount * revenue
    cancel_vat <- vat_amount * discount
    receipt <- receivable - cancel_rev - cancel_vat 
    entries[[5]] <- tibble::tibble(
      date = rep(date+dso-earliness,4),
      label = rep(label_cashin,4),
      account = c(acc_cash,acc_disc,acc_vatx,acc_rece),
      debit = c(receipt, cancel_rev, cancel_vat,NA),
      credit = c(NA,NA,NA,receivable)
    )
    
  }
  
  entries <- dplyr::bind_rows(entries)
  
  return(entries)
}

