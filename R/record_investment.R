#' Write investment entries in the ledger.
#' @param date        Date. date of the sale.
#' @param object      Character. Name of the product or service sold.
#' @param price       Double. Base price of the product or service.
#' @param rate        Double. Interest rate used for the operating lease.
#' @param duration    Integer. Number of periods (months).
#' @param origin      Character. Account where the asset is "stored".
#' @param destination Character. Account where the asset depreciation is accumulated.
#' @return A tibble of journal entries.
#' @importFrom dplyr case_when
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @importFrom lubridate as_date
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate days_in_month
#' @importFrom lubridate day
#' @importFrom dplyr bind_rows
#' @importFrom FinancialMath amort.period
#' @export


record_investment <- function(date = Sys.Date(),
                              object = "asset",
                              price = 120000,
                              rate = NA,
                              duration = 60,
                              origin = 15300,
                              destination = 92000){
  
  
  acc_opcash <- 10100
  acc_invcash <- 10200
  acc_accruedint <- 23300
  acc_leaseliab <- 26300
  acc_leaseint  <- 66300
  
  type_asset <- dplyr::case_when(
    origin < 15200 & origin >= 15100 ~ "land",
    origin < 15300 & origin >= 15200 ~ "building",
    origin < 15400 & origin >= 15300 ~ "equipment",
    origin < 15500 & origin >= 15400 ~ "vehicles",
    origin < 16000 & origin >= 15900 ~ "lease",
    TRUE ~ "nonoperating"
  )
  
  acc_depr <- as.numeric(stringr::str_replace(origin, "^15", "16"))
  
  entries <- list()
  
  
  if (type_asset == "land"){
    
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(paste0("purchase of ", object),2),
      account = c(origin, acc_invcash),
      debit = c(price,NA),
      credit = c(NA,price)
    )
    
  } else if (type_asset == "lease"){
    
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(paste0("operating lease of ", object),2),
      account = c(origin, acc_leaseliab),
      debit = c(price,NA),
      credit = c(NA,price)
    )
    
    
    label_depr <- paste0("depreciation of ", object)
    label_interest <- paste0("interest on ", object)
    label_payment <- paste0("payment for ", object)
    date_depr <- date
    
    for (i in 1:duration){
      
      lubridate::day(date_depr) <- 1
      lubridate::month(date_depr) <- lubridate::month(date_depr) + 1
      lubridate::day(date_depr) <- lubridate::days_in_month(date_depr)
      simfin <- FinancialMath::amort.period(Loan=price,n=duration,i=rate/12, t=i)
      payment <- simfin[2]
      interest <- simfin[6]
      reimburs <- simfin[7]
      entries[[i+1]] <- tibble::tibble(
        date = rep(date_depr,7),
        label = c(rep(label_depr,2),rep(label_interest,2),rep(label_payment,3)),
        account = c(destination,acc_depr,acc_leaseint,acc_accruedint,acc_leaseliab,acc_accruedint,acc_opcash),
        debit = c(reimburs,NA,interest,NA,reimburs,interest,NA),
        credit = c(NA,reimburs,NA,interest,NA,NA,payment)
      )
      
    }
    
  } else {
    
    entries[[1]] <- tibble::tibble(
      date = rep(date,2),
      label = rep(paste0("purchase of ", object),2),
      account = c(origin, acc_invcash),
      debit = c(price, NA),
      credit = c(NA, price)
    )
    
    depreciation <- (price) / duration
    label_depr <- paste0("depreciation of ", object)
    date_depr <- date
    
    for (i in 1:duration){
      
      lubridate::day(date_depr) <- 1
      lubridate::month(date_depr) <- lubridate::month(date_depr) + 1
      lubridate::day(date_depr) <- lubridate::days_in_month(date_depr)
      entries[[i+1]] <- tibble::tibble(
        date = rep(date_depr,2),
        label = rep(label_depr,2),
        account = c(destination, acc_depr),
        debit = c(depreciation, NA),
        credit = c(NA, depreciation)
      )
      
    }
    
  }
  
  entries <- dplyr::bind_rows(entries)
  
  return(entries)
}