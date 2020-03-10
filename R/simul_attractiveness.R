#' Compute attractiveness scores of references based on prime costs, advertising and price.
#' @param profiles                Tibble. One observation for each reference (company-product) on the market, and the following properties: "reference","prime_cost","advertising", "price" and "dso".
#' @param sensitivity_prime_cost  Numeric. Relative weight customers put on prime cost.
#' @param sensitivity_advertising Numeric. Relative weight customers put on prime advertising.
#' @param sensitivity_price       Numeric. Relative weight customers put on prime price.
#' @param sensitivity_dso         Numeric. Relative weight customers put on DSO (credit terms).
#' @return A tibble with the relative attractiveness of each reference
#' @seealso simul_market
#' @export



simul_attractiveness <- function(profiles,
                                 sensitivity_prime_cost = 2,
                                 sensitivity_advertising = 2,
                                 sensitivity_price = 3,
                                 sensitivity_dso = 1){
  
  stopifnot(
    "reference" %in% names(profiles),
    "prime_cost" %in% names(profiles), 
    "advertising" %in% names(profiles),
    "price" %in% names(profiles),
    "dso" %in% names(profiles),
    is.numeric(sensitivity_prime_cost),
    is.numeric(sensitivity_advertising),
    is.numeric(sensitivity_price)
  )
  
  profiles$prime_cost <- sensitivity_prime_cost * ((profiles$prime_cost - min(profiles$prime_cost)) / (max(profiles$prime_cost) - min(profiles$prime_cost)))
  profiles$advertising <- sensitivity_advertising * ((profiles$advertising - min(profiles$advertising)) / (max(profiles$advertising) - min(profiles$advertising)))
  profiles$price <- sensitivity_price * ((profiles$price - min(profiles$price)) / (max(profiles$price) - min(profiles$price)))
  profiles$dso <- sensitivity_dso * ((profiles$dso - min(profiles$dso)) / (max(profiles$dso) - min(profiles$dso)))
  
  profiles$attractiveness <- (profiles$prime_cost + profiles$advertising + profiles$price + profiles$dso) / (sensitivity_prime_cost + sensitivity_advertising + sensitivity_price + sensitivity_dso)
  profiles$attractiveness <- round(profiles$attractiveness*100,0)
  
  
  profiles <- profiles[,c("reference","attractiveness")]
  
  return(profiles)
}

