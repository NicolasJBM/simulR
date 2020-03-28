#' Write pool reset entries in the ledger.
#' @param date        Date. date of the consumption.
#' @param object      Character. Name of the materials consumed.
#' @param amount      Double. Amount empryin the cost pool or cost object.
#' @param origin      Character. From where the consumption is made.
#' @param destination Character. Where the consumption goes.
#' @return A tibble of journal entries.
#' @importFrom tibble tibble
#' @export


record_pool_reset <- function(date, object, amount, origin, destination){
  
  if (amount < 0){
    altorigin <- destination
    destination <- origin
    origin <- altorigin
    amount = abs(amount)
    specification <- " - over-application"
  } else specification <- " - under-application"
  
  
  entries <- tibble::tibble(
    date = rep(date, 2),
    label = paste0(object, specification),
    account =  c(destination, origin),
    debit = c(amount, NA),
    credit = c(NA, amount)
  )
  
  return(entries)
}