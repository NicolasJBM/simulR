#' Create periods for the specified duration.
#' @param date Date. date of the sale.
#' @return A tibble with different specifications of the period
#' @importFrom lubridate as_date
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate days_in_month
#' @export


create_period <- function(date){
  
  date <- lubridate::as_date(date)
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  lastday <- lubridate::days_in_month(date)
  month_chr <- dplyr::case_when(nchar(month) == 1 ~ paste0("-0",month), TRUE ~ paste0("-",month))
  
  tibble(
    date = date,
    period = paste0("P-",year, month_chr),
    year = year,
    month = month,
    lastday = lubridate::as_date(paste0(year, "-", month, "-", lastday))
  )
  
}