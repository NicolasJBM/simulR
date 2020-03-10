#' Simulate a base market over several periods based on a set of assumptions.
#' @param market_base     Integer. Base volume of the demand.
#' @param profiles        Tibble. One row per reference (company-product), its name and its attractiveness (output of the function simul_attractiveness).
#' @param demand_function Character. Whether the demand is a "linear", "constant" elasiticy, or "logistic" function of price.
#' @return A volume of the demand to the firm.
#' @seealso simul_market
#' @export


simul_demand <- function(market_base = 1000,
                         profiles = NULL,
                         demand_function = "logistic"){
  
  stopifnot(
    is.numeric(market_base),
    !is.null(profiles),
    demand_function %in% c("linear","constant","logistic")
  )
  
  linear <- function(p, alpha, beta) alpha*p + beta
  constant <- function(p, alpha, beta) exp(alpha*log(p)+beta)
  logistic <- function(p, c, alpha, p0) c/(1+exp(-alpha*(p-p0)))
  
  
  coefficient <- c()
  
  for (i in profiles$attractiveness){
    j <- switch(
      demand_function,
      linear = linear(i, -1, 100)+25,
      constant = constant(i, -0.3, 4.75)+10,
      logistic = logistic(i, 100, -.2, 50)+25
    )
    
    coefficient <- c(coefficient, j)
  }
  
  profiles$demand <- round(market_base * coefficient / 1000)*10
  
  return(profiles)
}
