#' Write entries in the ledger related to investments.
#' @param date        Date. date of the sale.
#' @param object      Character. Name of the product or service sold.
#' @param quantity    Integer. Quantity emitted.
#' @param price       Double. Base price of the product or service.
#' @param rate        Double. Interest rate used for the operating lease.
#' @param lifetime    Integer. Number of periods (months).
#' @param nature      Character. Account where the asset is "stored".
#' @param destination Character. Account where the asset depreciation is accumulated.
#' @return A tibble of journal entries.
#' @importFrom dplyr case_when
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @importFrom lubridate as_date
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate days_in_month
#' @importFrom lubridate day
#' @importFrom dplyr bind_rows
#' @importFrom FinancialMath amort.period
#' @export


record_financing <- function(date = Sys.Date(),
                             object = "bond",
                             quantity = 100,
                             price = 100,
                             rate = 0.05,
                             lifetime = 24,
                             nature = 26400,
                             destination = 10300){
  
  
  acc_opcash <- 10100
  acc_fincash <- 10300
  acc_accruednote <- 23100
  acc_accruedmort <- 23200
  acc_accruedbond <- 23400
  acc_accrueddiv <- 23900
  acc_intnote <- 66100
  acc_intmort <- 66200
  acc_intbond <- 66400
  
  
  type_fin <- dplyr::case_when(
    nature < 25100 & nature >= 25000  ~ "note",
    nature < 26300  & nature >= 26200 ~ "mortgage",
    nature < 26500 & nature >= 26400 ~ "bond",
    nature < 32000 & nature >= 31000 ~ "common",
    nature < 33000  & nature >= 32000 ~ "preferred",
    nature < 34000 & nature >= 33000 ~ "paidin",
    TRUE ~ "dividend"
  )
  
  
  entries <- list()
  
  
  if (type_fin == "note"){
    
    note_amount <- quantity * price
    date_int <- date
    day_date <- lubridate::day(date)
    lubridate::day(date_int) <- 01
    lubridate::month(date_int) <- lubridate::month(date_int) + lifetime
    lubridate::day(date_reimb) <- min(lubridate::days_in_month(date_int),day_date) 
    interest <- note_amount * lifetime * rate / 12
    
    label_note <- paste0("note contracted on ", date, " and due on ", date_reimb)
    label_int <- paste0("interest on the ", label_note)
    label_reimb <- paste0("reimbursment of the ", label_note)
    
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(label_note,2),
      account = c(acc_fincash,nature),
      debit = c(note_amount,NA),
      credit = c(NA,note_amount)
    )
    
    entries[[2]] <- tibble::tibble(
      date = rep(date_int,2),
      label = rep(label_int,2),
      account = c(acc_intnote,acc_accruednote),
      debit = c(interest,NA),
      credit = c(NA,interest)
    )
    
    entries[[3]] <- tibble::tibble(
      date = rep(date_reimb,4),
      label = rep(label_int,4),
      account = c(nature,acc_accruednote,acc_fincash,acc_opcash),
      debit = c(note_amount,interest,NA,NA),
      credit = c(NA,NA,note_amount,interest)
    )
    
    
  } else if(type_fin == "mortgage"){
    
    mortgage_amount <- quantity * price
    label_mort <- paste0("mortage contracted on ", date)
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(label_mort,2),
      account = c(acc_fincash, nature),
      debit = c(mortgage_amount,NA),
      credit = c(NA,mortgage_amount)
    )
    
    label_interest <- paste0("interest on the ", label_mort)
    label_reimb <- paste0("reimbursment of the ", label_mort)
    date_reimb <- date
    
    for (i in 1:lifetime){
      
      lubridate::day(date_reimb) <- 1
      lubridate::month(date_reimb) <- lubridate::month(date_reimb) + 1
      lubridate::day(date_reimb) <- lubridate::days_in_month(date_reimb)
      simfin <- FinancialMath::amort.period(Loan=mortgage_amount,n=lifetime,i=rate/12, t=i)
      payment <- simfin[2]
      interest <- simfin[6]
      reimburs <- simfin[7]
      entries[[i+1]] <- tibble::tibble(
        date = rep(date_reimb,4),
        label = c(rep(label_reimb,2),rep(label_interest,2)),
        account = c(nature,acc_fincash,acc_intmort,acc_opcash),
        debit = c(payment,NA,interest,NA),
        credit = c(NA,payment,NA,interest)
      )
      
    }
    
  } else if(type_fin == "bond"){
    
    bond_amount <- quantity * price
    label_bond <- paste0(quantity, " bonds contracted on ", date, " at a face value of ", price, " and a yearly interest rate of ", rate, " over ", lifetime, " month.")
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(label_bond,2),
      account = c(acc_fincash, nature),
      debit = c(bond_amount,NA),
      credit = c(NA,bond_amount)
    )
    
    label_interest <- paste0("interest on ", label_bond)
    label_reimb <- paste0("reimbursment of ", label_bond)
    date_pay <- date
    interest <- bond_amount * rate / 12
    
    for (i in 1:lifetime){
      
      lubridate::day(date_pay) <- 1
      lubridate::month(date_pay) <- lubridate::month(date_pay) + 1
      lubridate::day(date_pay) <- lubridate::days_in_month(date_pay)
      
      entries[[i+1]] <- tibble::tibble(
        date = rep(date_pay,2),
        label = c(rep(label_interest,2)),
        account = c(acc_intbond, acc_opcash),
        debit = c(interest,NA),
        credit = c(NA,interest)
      )
      
      if (i == lifetime){
        
        entries[[i+2]] <- tibble::tibble(
          date = rep(date_pay,2),
          label = c(rep(label_reimb,2)),
          account = c(nature, acc_fincash),
          debit = c(bond_amount,NA),
          credit = c(NA,bond_amount)
        )
        
      }
      
    }
    
    
  } else if(type_fin == "common"){
    
    common_amount <- quantity * price
    label_common <- paste0(quantity, " common stock shares contracted on ", date, " at a face value of ", price)
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(label_common,2),
      account = c(acc_fincash, nature),
      debit = c(common_amount,NA),
      credit = c(NA,common_amount)
    )
    
  } else if(type_fin == "preferred"){
    
    preferred_amount <- quantity * price
    label_preferred <- paste0(quantity, " preferred stock shares contracted on ", date, " at a face value of ", price)
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(label_preferred,2),
      account = c(acc_fincash, nature),
      debit = c(preferred_amount,NA),
      credit = c(NA,preferred_amount)
    )
    
  } else if(type_fin == "paidin"){
    
    paidin_amount <- quantity * price
    label_paidin <- paste0(quantity, " paid-in capital shares contracted on ", date, " at a face value of ", price)
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(label_paidin,2),
      account = c(acc_fincash, nature),
      debit = c(paidin_amount,NA),
      credit = c(NA,paidin_amount)
    )
    
  } else {
    
    dividend <- quantity * price
    date_pay <- date
    lubridate::day(date_pay) <- 1
    lubridate::month(date_pay) <- lubridate::month(date_pay) + 1
    lubridate::day(date_pay) <- lubridate::days_in_month(date_pay)
    entries[[1]] <- tibble::tibble(
      date = c(rep(date,2),rep(date_pay,2)),
      label = c(rep("declare dividend",2), rep("pay dividend",2)),
      account = c(nature,acc_accrueddiv,acc_accrueddiv,acc_fincash),
      debit = c(dividend,NA,dividend,NA),
      credit = c(NA,dividend,NA,dividend)
    )
    
    
  }
  
  entries <- dplyr::bind_rows(entries)
  
  return(entries)
}
