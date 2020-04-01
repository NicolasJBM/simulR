#' Write salary entries in the ledger.
#' @param date        Date. date of the recognition and payment.
#' @param object      Character. Name of the kind of employees.
#' @param quantity    Integer. Time paid in Full Time Equivalent.
#' @param price       Double. Monthly salary of the workfroce.
#' @param rate        Double. Percentage of base salary to compute labor taxes.
#' @param origin      Character. From where the salary is taken.
#' @param destination Character. Where the salary goes.
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


record_salary <- function(date = Sys.Date(),
                          object = "salespeople",
                          quantity = 1,
                          price = 2000,
                          rate = 0.5,
                          origin = 22100,
                          destination = 61200){
  
  
  acc_opcash <- 10010
  acc_tax <- 24200
  
  wages <- price * quantity
  taxes <- wages * rate
  cost <- wages + taxes
  label_record <- paste0(object, " for ", quantity, " full-time equivalent")
  label_payment <- paste0("payment of", object, " for ", quantity, "full-time equivalent")
  
  entries <- list()
  
  entries[[1]] <- tibble::tibble(
    date = rep(date,3),
    label = rep(label_record,3),
    account = c(destination,origin,acc_tax),
    debit = c(cost,NA,NA),
    credit = c(NA,wages,taxes)
  )
  
  entries[[2]] <- tibble::tibble(
    date = rep(date,2),
    label = rep(label_record,2),
    account = c(origin,acc_opcash),
    debit = c(wages,NA),
    credit = c(NA,wages)
  )
  
  entries <- dplyr::bind_rows(entries)
  
  return(entries)
}

