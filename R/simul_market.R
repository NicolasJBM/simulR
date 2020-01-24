#' Simulate a base market over several periods based on a set of assumptions.
#' @param start Character or date. When the the time series should start.
#' @param years Integer. How many years should the time series last.
#' @param base_volume Integer. Set magnitude for average daily firm demand.
#' @param trend Double. Linear trader across all periods.
#' @param randomness Double. Percentage of random variation for the demand.
#' @param coeffday Tibble. 7 observations for 2 variables: the week day (short) and the distribution (higher numbers indicate on which days demand should be concentrated)
#' @param coeffmonth  Tibble. 12 observations for 2 variables: the month (number) and the distribution (higher numbers indicate on which months demand should be concentrated)
#' @return A tibble with the market size for eachh period.
#' @importFrom chron seq.dates
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate week
#' @importFrom lubridate day
#' @importFrom lubridate mdy
#' @export


simul_market <- function(start = Sys.Date(),
                         years = 5,
                         base_volume = 1000,
                         trend = 0.05,
                         randomness = 0.10,
                         coeffday = NULL,
                         coeffmonth = NULL){
  
  stopifnot(
    is.numeric(years),
    is.numeric(base_volume),
    is.numeric(trend) & trend > -1 & trend < 1,
    is.numeric(randomness) & randomness >= 0,
    length(coeffday) == 2,
    length(coeffmonth) == 2
  )
  
  coeffday$coeffday <- 7 * coeffday$coeffday/sum(coeffday$coeffday)
  coeffmonth$coeffmonth <- 12 * coeffmonth$coeffmonth/sum(coeffmonth$coeffmonth)
  
  
  market <- data.frame(
    date = chron::seq.dates(
      paste(lubridate::month(start), lubridate::day(start), lubridate::year(start), sep = "/"),
      paste(lubridate::month(start), lubridate::day(start), (lubridate::year(start)+years), sep = "/")
    )
  )
  
  market$year <- lubridate::year(lubridate::mdy(market$date))
  market$month <- lubridate::month(lubridate::mdy(market$date))
  market$week <- lubridate::week(lubridate::mdy(market$date))
  market$day <- lubridate::day(lubridate::mdy(market$date))
  market$weekday <- weekdays(lubridate::mdy(market$date))
  market$sequence <- 1:length(market$date)
  
  market <- merge(market, coeffday, by = "weekday")
  market <- merge(market, coeffmonth, by = "month")
  market <- market[order(market$sequence),]
  
  market$market <- ((base_volume * market$coeffmonth * market$coeffday) + market$sequence * trend)
  market$market[market$market<0] <- 0
  market$market <- sapply(market$market, wiggle, delta = randomness)
  market$market <- floor(market$market)
  
  market <- market[, c("sequence","date","year","month","week","day","weekday","market")]
  
  return(market)
}
