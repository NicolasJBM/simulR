#' Write consumption entries in the ledger.
#' @param date        Date. date of the consumption.
#' @param object      Character. Name of the materials consumed.
#' @param quantity    Integer. Volume consumed.
#' @param price       Double. Base price of the materials consumed.
#' @param origin      Character. From where the consumption is made.
#' @param destination Character. Where the consumption goes.
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



record_transfer <- function(date = Sys.Date(),
                               object = "materials",
                               quantity = 200,
                               price = 3,
                               origin = 13300,
                               destination = 91000){
  
  
  consumption <- price * quantity
  label <- object
  
  entries <- list()
  
  entries[[1]] <- tibble::tibble(
    date = rep(date,2),
    label = rep(label,2),
    account = c(destination,origin),
    debit = c(consumption,NA),
    credit = c(NA,consumption)
  )
  
  entries <- dplyr::bind_rows(entries)
  
  return(entries)
}