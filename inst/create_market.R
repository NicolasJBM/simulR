#' Simulate a base market over several periods based on a set of assumptions.
#' @param start       Character or date. When the the time series should start.
#' @param years       Integer. How many years should the time series last.
#' @param base_volume Integer. Set magnitude for average daily firm demand.
#' @param trend       Double. Linear trader across all periods.
#' @param randomness  Double. Percentage of random variation for the demand.
#' @param seasons     Tibble. "week", "weekday" and "coefficient" indicating the distribution.
#' @param periodicity Character. Whether the market should be defined at the "year", "month" or "day" level.
#' @return A tibble with the market size for eachh period.
#' @importFrom chron seq.dates
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate week
#' @importFrom lubridate day
#' @importFrom lubridate mdy
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
#' @export


create_market <- function(start = Sys.Date(),
                          years = 5,
                          base_volume = 1000,
                          trend = 1/20000,
                          randomness = 0.10,
                          seasons = NULL,
                          periodicity = "month"){
  
  stopifnot(
    is.numeric(years),
    is.numeric(base_volume),
    is.numeric(trend) & trend > -1 & trend < 1,
    is.numeric(randomness) & randomness >= 0,
    names(seasons) == c("week","weekday","coefficient"),
    periodicity %in% c("year","month","day")
  )
  
  
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
  market$weekday <- tolower(weekdays(lubridate::mdy(market$date)))
  market$sequence <- 1:length(market$date)
  
  if (!is.null(seasons)){
    market <- dplyr::left_join(market, seasons, by = c("week","weekday"))
  } else {
    market$coefficient <- 1
  }
  
  market <- market[order(market$sequence),]
  
  market$market <- ((base_volume * market$coefficient)*(1+trend*market$sequence))
  market$market[market$market<0] <- 0
  market$market <- sapply(market$market, wiggle, delta = randomness)
  market$market <- floor(market$market)
  
  market <- market[, c("sequence","date","year","month","week","day","weekday","market")]
  
  
  if (periodicity == "year"){
    
    market <- market %>%
      dplyr::group_by(year) %>%
      dplyr::filter(market > 0) %>%
      dplyr::summarise(market = sum(market), working_days = n()) %>%
      dplyr::mutate(period = as.character(year)) %>%
      dplyr::ungroup() %>%
      dplyr::select(period, market, working_days) %>%
      dplyr::filter(working_days > 200)
    
  } else if(periodicity == "month"){
    
    market <- market %>%
      dplyr::group_by(year, month) %>%
      dplyr::filter(market > 0) %>%
      dplyr::summarise(market = sum(market), working_days = n()) %>%
      dplyr::mutate(period = dplyr::case_when(
        nchar(month) == 1 ~ paste0(year, "-0", month),
        TRUE ~ paste0(year, "-", month)
      )) %>%
      dplyr::ungroup() %>%
      dplyr::select(period, market, working_days) %>%
      dplyr::filter(working_days > 15)
    
  } else {
    
    market <- market %>%
      dplyr::filter(market > 0) %>%
      dplyr::mutate(working_days = 1) %>%
      dplyr::mutate(period = dplyr::case_when(
        nchar(month) == 1 ~ paste0(year, "-0", month),
        TRUE ~ paste0(year, "-", month)
      ))  %>%
      dplyr::mutate(period = dplyr::case_when(
        nchar(day) == 1 ~ paste0(period, "-0", day),
        TRUE ~ paste0(period, "-", day)
      )) %>%
      dplyr::select(period, market, working_days)
    
  }
  
  
  return(market)
}
