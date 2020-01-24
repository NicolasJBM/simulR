#' Return a new value within plus of minus delta percent of the initial value, adjusting roundings based on the initial magnitude.
#' @param vol    Integer. Volumes to be split in batches.
#' @param date   Date. Date of the volumes.
#' @param nbatch Number of batches to create.
#' @param shape  Numeric. Positive number; low numbers means skewed to he left, high number to the right.
#' @return A dataframe with lots and their respective volumes.
#' @importFrom lubridate mdy
#' @importFrom stats rgamma
#' @export




make_batches <- function(vol, date, nbatch = 50, shape = 5){
  
  batches <-  data.frame(batches = rgamma(n = vol, shape = 5))
  batches$batches <- (batches$batches - min(batches$batches)) / (max(batches$batches) - min(batches$batches))
  batches$batches <- round(batches$batches*50, 0)
  batches <- as.data.frame(table(batches$batches))
  names(batches) <- c("lot","quantity")
  batches$lot <- paste(lubridate::mdy(date), batches$lot, sep = "_")
  
  return(batches)
}