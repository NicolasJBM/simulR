#' Split a volume into dated batches.
#' @param object         Character. component of the final label identifying the batch.
#' @param quantity       Integer. Number of units to be divided in batches.
#' @param max_batch_nbr  Integer. Number of batches.
#' @param min_batch_size Integer. Minimum number of units in a batch.
#' @param start_date     Date. Beginning of the period.
#' @param end_date       Date. End of the period.
#' @return A dataframe with date, object and quantity of each batch.
#' @importFrom lubridate day
#' @importFrom stats rnorm
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr select
#' @export


create_batches <- function(object = "lot",
                           quantity = 1200,
                           max_batch_nbr = 20,
                           min_batch_size = 20,
                           start_date = Sys.Date(),
                           end_date = Sys.Date() + 20){
  
  
  # Bind variables
  Freq <- NULL
  lot <- NULL
  
  days <- 2:(as.numeric(end_date-start_date)-1)
  dates <- rep(start_date, max_batch_nbr)
  for (i in 1:length(dates)) lubridate::day(dates[[i]]) <- sample(days,1, replace = TRUE)
  
  batches <-  data.frame(batches = stats::rnorm(n = quantity))
  batches$batches <- (batches$batches - min(batches$batches)) / (max(batches$batches) - min(batches$batches))
  batches$batches <- round(batches$batches*max_batch_nbr, 0)
  batches <- as.data.frame(table(batches$batches)) %>%
    dplyr::mutate(lot = dplyr::case_when(Freq < min_batch_size ~ "0", TRUE ~ as.character(Var1))) %>%
    dplyr::group_by(lot) %>%
    dplyr::summarise(quantity = sum(Freq))
  
  batches <- batches %>%
    dplyr::mutate(
      date = sample(dates, nrow(batches)),
      object = paste0(object, " - B", lot)
    ) %>%
    dplyr::select(date, object, quantity)
  
  return(batches)
}
