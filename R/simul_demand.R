#' Simulate a base market over several periods based on a set of assumptions.
#' @param market_base     Integer. Base volume of the demand.
#' @param prices          Numeric vector. Prices applied for each product.
#' @param qualities       Numeric vector. Relative qualities of each product.
#' @param sensitivity     Numeric. Number between 0 (less sensitive) and 1 (more sensitive) indicating how sentitive to quality price demand is.
#' @param demand_function Character. Whether the demand is a "linear", "constant" elasiticy, or "logistic" function of price.
#' @return A volume of the demand to the firm.
#' @seealso simul_market
#' @export


simul_demand <- function(market_base = 1000,
                         prices =    c(25,50,75,100),
                         qualities = c(20,55,65,105),
                         sensitivity = 0.5,
                         demand_function = "logistic"){
  
  stopifnot(
    is.numeric(market_base),
    is.numeric(prices),
    is.numeric(qualities),
    length(prices) == length(qualities),
    demand_function %in% c("linear","constant","logistic")
  )
  
  linear <- function(p, alpha, beta) alpha*p + beta
  constant <- function(p, alpha, beta) exp(alpha*log(p)+beta)
  logistic <- function(p, c, alpha, p0) c/(1+exp(-alpha*(p-p0)))
  
  qp <- prices / qualities
  qp <- 100 * (qp - min(qp)) / (max(qp) - min(qp))
  qp <- qp * sensitivity + (1-sensitivity)*100
  
  coefficient <- c()
  
  for (i in qp){
    j <- switch(
      demand_function,
      linear = linear(i, -1, 100)+25,
      constant = constant(i, -0.3, 4.75)+10,
      logistic = logistic(i, 100, -.2, 50)+25
    )
    
    coefficient <- c(coefficient, j)
  }
  
  demand <- round(market_base * coefficient / 1000)*10
  
  return(demand)
}
