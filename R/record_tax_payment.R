#' Write tax payment entries in the ledger.
#' @param date        Date. date of the consumption.
#' @param object      Character. Name of the materials consumed.
#' @param amount      Double. Amount due to the State.
#' @param origin      Character. From where the consumption is made.
#' @return A tibble of journal entries.
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @export





record_tax_payment <- function(date, object, amount, origin){
  
  acc_opcash <- 10010
  
  entries <- tibble::tibble(
    date = rep(date,2),
    label = rep(paste0("payment of ", object),2),
    account = c(origin,acc_opcash),
    debit = c(amount,NA),
    credit = c(NA,amount)
  )
  
  return(entries)
}