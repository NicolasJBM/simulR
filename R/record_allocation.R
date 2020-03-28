#' Write allocation entries in the ledger.
#' @param date            Date. date of the consumption.
#' @param object          Character. Name of the materials consumed.
#' @param allocation_base Double. Number of units of allocation base.
#' @param allocation_rate Double. Amount allocated for each unit of allocation base.
#' @param origin          Character. From where the consumption is made.
#' @param destination Character. Where the consumption goes.
#' @return A tibble of journal entries.
#' @importFrom tibble tibble
#' @export


record_allocation <- function(date, object, allocation_base, allocation_rate, origin, destination){
  
  allocated = round(allocation_base * allocation_rate, 2)
  
  entries <- tibble::tibble(
    date = rep(date, 2),
    label = rep(object, 2),
    account = c(destination, origin),
    debit = c(allocated, NA),
    credit = c(NA, allocated)
  )
  
  return(entries)
}