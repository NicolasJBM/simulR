#' Simulate a base market over several periods based on a set of assumptions.
#' @param start         Character or date. When the the time series should start.
#' @param number_months Integer. How many months should the time series last.
#' @param base_volume   Integer. Set magnitude for average daily firm demand.
#' @param trend         Double. Linear trader across all periods.
#' @param randomness    Double. Percentage of random variation for the demand.
#' @param seasons       Tibble. "week", "weekday" and "coefficient" indicating the distribution.
#' @return A tibble with the market size (forcast and actual) for each period.
#' @importFrom chron seq.dates
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate week
#' @importFrom lubridate day
#' @importFrom lubridate mdy
#' @importFrom lubridate %m+%
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#' @importFrom purrr map_dbl
#' @export


create_market <- function(start = Sys.Date(),
                          number_months = 60,
                          base_volume = 1000,
                          trend = 1/20000,
                          randomness = 0.10,
                          seasons = NULL){
  
  stopifnot(
    is.numeric(number_months),
    is.numeric(base_volume),
    is.numeric(trend) & trend > -1 & trend < 1,
    is.numeric(randomness) & randomness >= 0,
    names(seasons) == c("week","weekday","coefficient")
  )
  
  
  # bind variables
  actual <- NULL
  working_day <- NULL
  weekday <- NULL
  
  
  market <- data.frame(
    date = chron::seq.dates(
      paste(lubridate::month(start), lubridate::day(start), lubridate::year(start), sep = "/"),
      paste(lubridate::month(start %m+% months(number_months)), lubridate::day(start), (lubridate::year(start %m+% months(number_months))), sep = "/")
    )
  )
  
  market$year <- lubridate::year(lubridate::mdy(market$date))
  market$month <- lubridate::month(lubridate::mdy(market$date))
  market$week <- lubridate::week(lubridate::mdy(market$date))
  market$day <- lubridate::day(lubridate::mdy(market$date))
  market$weekday <- tolower(weekdays(lubridate::mdy(market$date)))
  market$sequence <- 1:length(market$date)
  
  if (!is.null(seasons)){
    market <- dplyr::left_join(market, seasons, by = c("week","weekday"))
  } else {
    market$coefficient <- 1
  }
  
  market <- market[order(market$sequence),]
  
  market$coefficient <- market$coefficient*(1+trend*market$sequence)
  market$coefficient <- (market$coefficient-min(market$coefficient))/(max(market$coefficient)-(min(market$coefficient)))
  market$market <- base_volume * market$coefficient
  market$market[market$market < 0] <- 0
  market$market <- sapply(market$market, wiggle, delta = randomness)
  market$market <- floor(market$market)
  
  market <- market[, c("sequence","date","year","month","week","day","weekday","market")]
  
  market <- market %>%
    dplyr::mutate(working_day = as.numeric(market > 0)) %>%
    dplyr::select(date, market, weekday, working_day) %>%
    dplyr::mutate(actual = purrr::map_dbl(market, simulR::wiggle, delta = 0.2)) %>%
    dplyr::mutate(actual = round(actual,0)) %>%
    dplyr::select(date, weekday, working_day, forecast = market, actual)
  
  return(market)
}
