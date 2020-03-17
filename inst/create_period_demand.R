#' Simulate the distribution of the demand for one period based on relative competitiveness profiles.
#' @param market_base     Integer. Base volume of the demand.
#' @param profiles        Tibble. One row per reference (company-product), its name and its attractiveness (output of the function simul_attractiveness).
#' @param demand_function Character. Whether the demand is a "linear", "constant" elasiticy, or "logistic" function of price.
#' @param sensitivity     Double. Number between 0 and 1 indicating how responsive the demand should be to attractiveness.
#' @return A volume of the demand to the firm.
#' @seealso simul_market
#' @export


create_period_demand <- function(market_base = 1000,
                                 profiles = NULL,
                                 demand_function = "logistic",
                                 sensitivity = 0.5){
  
  stopifnot(
    is.numeric(market_base),
    !is.null(profiles),
    demand_function %in% c("linear","constant","logistic"),
    sensitivity >= 0 & sensitivity <= 1
  )
  
  linear <- function(p, alpha, beta) alpha*p + beta
  constant <- function(p, alpha, beta) exp(alpha*log(p)+beta)
  logistic <- function(p, c, alpha, p0) c/(1+exp(-alpha*(p-p0)))
  
  
  coefficient <- c()
  
  for (i in profiles$attractiveness){
    j <- switch(
      demand_function,
      linear = linear((100-i*sensitivity), -1, 100)+25,
      constant = constant((100-i*sensitivity), -0.3, 4.75)+10,
      logistic = logistic((100-i*sensitivity), 100, -.2, 50)+25
    )
    
    coefficient <- c(coefficient, j)
  }
  
  coefficient <- coefficient / sum(coefficient)
  profiles$demand <- round(market_base * coefficient / 10)*10
  
  
  
  batches <-  data.frame(batches = rgamma(n = vol, shape = shape))
  batches$batches <- (batches$batches - min(batches$batches)) / (max(batches$batches) - min(batches$batches))
  batches$batches <- round(batches$batches*nbatch, 0)
  batches <- as.data.frame(table(batches$batches))
  names(batches) <- c("lot","quantity")
  
  
  return(profiles)
}
