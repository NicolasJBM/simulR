#' Return a new value within plus of minus delta percent of the initial value, adjusting roundings based on the initial magnitude.
#' @param x Numeric. Value around which to wiggle
#' @param delta Numeric. percentage of variation arounf the initial value.
#' @param dir Numeric. -1 for negative, 1 for positive, 0 for either.
#' @return A new value
#' @importFrom dplyr case_when
#' @export


wiggle <- function(x, delta = 0.2, dir = 0){
  
  x <- as.numeric(x)
  xscale <- nchar(as.character(ceiling(x)))
  
  incr <- dplyr::case_when(
    x < 0.1 ~ 0.001,
    x < 1 ~ 0.01,
    xscale == 1 ~ 0.1,
    xscale == 2 ~ 0.5,
    xscale == 3 ~ 1,
    xscale == 4 ~ 10,
    TRUE ~ 100
    )
  
  change <- dplyr::case_when(
    x < 0.1     ~  round(x * delta,4),
    xscale == 1 ~  round(x * delta,2),
    xscale == 2 ~  round(x * delta,1),
    xscale == 3 ~  round(x * delta,0),
    xscale == 4 ~  round(x * delta / 10,0) * 10,
    TRUE ~ round(x * delta / 100,0) * 100
  )
  
  if (dir == 1) less <- 0 else less <- change
  if (dir == -1) plus <- 0 else plus <- change
  
  if (x != 0) y <- sample(setdiff(seq(from = x - less, to = x + plus, by = incr), x), 1) else y <- 0
  
  return(y)
}
