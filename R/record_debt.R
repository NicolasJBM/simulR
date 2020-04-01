#' Write debt financing entries in the ledger.
#' @param date        Date. date of the loan.
#' @param object      Character. Name of the loan.
#' @param quantity    Integer. Quantity emitted (for bonds; 1 otherwise).
#' @param price       Double. Amount borrowed per unit.
#' @param rate        Double. Interest rate used.
#' @param duration    Integer. Number of periods (months).
#' @param origin      Character. Account where the debt is accounted for.
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


record_debt <- function(date = Sys.Date(),
                             object = "bond",
                             quantity = 100,
                             price = 100,
                             rate = 0.05,
                             duration = 24,
                             origin = 26400){
  
  
  acc_opcash <- 10010
  acc_fincash <- 10030
  acc_accruednote <- 23100
  acc_accruedmort <- 23200
  acc_accruedbond <- 23400
  acc_intnote <- 66100
  acc_intmort <- 66200
  acc_intbond <- 66400
  
  
  type_fin <- dplyr::case_when(
    origin < 26200 & origin >= 26100  ~ "note",
    origin < 26300  & origin >= 26200 ~ "mortgage",
    origin < 26500 & origin >= 26400 ~ "bond"
  )
  
  
  entries <- list()
  
  
  if (type_fin == "note"){
    
    note_amount <- quantity * price
    date_int <- date
    day_date <- lubridate::day(date)
    lubridate::day(date_int) <- 01
    lubridate::month(date_int) <- lubridate::month(date_int) + duration
    lubridate::day(date_reimb) <- min(lubridate::days_in_month(date_int),day_date) 
    interest <- note_amount * duration * rate / 12
    
    label_note <- paste0("note contracted on ", date, " and due on ", date_reimb)
    label_int <- paste0("interest on the ", label_note)
    label_reimb <- paste0("reimbursment of the ", label_note)
    
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(label_note,2),
      account = c(acc_fincash,origin),
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
      account = c(origin,acc_accruednote,acc_fincash,acc_opcash),
      debit = c(note_amount,interest,NA,NA),
      credit = c(NA,NA,note_amount,interest)
    )
    
    
  } else if(type_fin == "mortgage"){
    
    mortgage_amount <- quantity * price
    label_mort <- paste0("mortage contracted on ", date)
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(label_mort,2),
      account = c(acc_fincash, origin),
      debit = c(mortgage_amount,NA),
      credit = c(NA,mortgage_amount)
    )
    
    label_interest <- paste0("interest on the ", label_mort)
    label_reimb <- paste0("reimbursment of the ", label_mort)
    date_reimb <- date
    
    for (i in 1:duration){
      
      lubridate::day(date_reimb) <- 1
      lubridate::month(date_reimb) <- lubridate::month(date_reimb) + 1
      lubridate::day(date_reimb) <- lubridate::days_in_month(date_reimb)
      simfin <- FinancialMath::amort.period(Loan=mortgage_amount,n=duration,i=rate/12, t=i)
      payment <- simfin[2]
      interest <- simfin[6]
      reimburs <- simfin[7]
      entries[[i+1]] <- tibble::tibble(
        date = rep(date_reimb,4),
        label = c(rep(label_reimb,2),rep(label_interest,2)),
        account = c(origin,acc_fincash,acc_intmort,acc_opcash),
        debit = c(payment,NA,interest,NA),
        credit = c(NA,payment,NA,interest)
      )
      
    }
    
  } else if(type_fin == "bond"){
    
    bond_amount <- quantity * price
    label_bond <- paste0(quantity, " bonds contracted on ", date, " at a face value of ", price, " and a yearly interest rate of ", rate, " over ", duration, " month.")
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(label_bond,2),
      account = c(acc_fincash, origin),
      debit = c(bond_amount,NA),
      credit = c(NA,bond_amount)
    )
    
    label_interest <- paste0("interest on ", label_bond)
    label_reimb <- paste0("reimbursment of ", label_bond)
    date_pay <- date
    interest <- bond_amount * rate / 12
    
    for (i in 1:duration){
      
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
      
      if (i == duration){
        
        entries[[i+2]] <- tibble::tibble(
          date = rep(date_pay,2),
          label = c(rep(label_reimb,2)),
          account = c(origin, acc_fincash),
          debit = c(bond_amount,NA),
          credit = c(NA,bond_amount)
        )
        
      }
      
    }
    
  }
  
  entries <- dplyr::bind_rows(entries)
  
  return(entries)
}
