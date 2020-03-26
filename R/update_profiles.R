#' Update information about companies' competitive profiles in competition. 
#' @param competition List. competitors as returned by the function make_competition.
#' @param simperiod   Character. ID of the period for which the profile holds.
#' @param simdemand      Integer. Demand for the period.
#' @param base_market List. market based returned by the function create_case.
#' @importFrom dplyr %>%
#' @importFrom dplyr filter 
#' @importFrom dplyr summarise
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr ungroup
#' @importFrom dplyr everything
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_wider 
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @importFrom stats na.omit
#' @return Append updated profiles to the competitors' parameters.
#' @export


update_profiles <- function(competition, simperiod, simdemand, base_market){
  
  
  # Bind variables
  account <- NULL
  advertising <- NULL 
  commission <- NULL 
  nature <- NULL 
  purpose <- NULL 
  cost <- NULL 
  destination <- NULL
  discount <- NULL 
  dso <- NULL
  input <- NULL
  input_standard_quantity <- NULL
  origin <- NULL
  desintation <- NULL
  output <- NULL
  output_standard_quantity <- NULL
  parameter <- NULL
  period <- NULL
  phase <- NULL
  price <- NULL
  quantity <- NULL
  resource_type <- NULL
  risk <- NULL
  sensitivity <- NULL
  sensitivity_advertising <- NULL
  sensitivity_commissions <- NULL
  sensitivity_dso <- NULL
  sensitivity_price <- NULL
  sensitivity_quality <- NULL
  value <- NULL
  
  
  
  profiles <- list()
  
  
  for (i in 1:length(competition)){
    
    # Gather decisions about price, dso, discounts
    tmp1 <- competition[[i]]$capacity %>%
      dplyr::filter(nature == "revenues") %>%
      dplyr::select(company, account = origin, parameter, value) %>%
      tidyr::pivot_wider(names_from = c("parameter"), values_from = c("value")) %>%
      dplyr::select(company, account, price, discount, dso)
    
    # Gather decisions about price, dso, discounts
    tmp2 <- competition[[i]]$capacity %>%
      dplyr::filter(nature == "commissions") %>%
      dplyr::select(company, account = destination, parameter, value) %>%
      tidyr::pivot_wider(names_from = c("parameter"), values_from = c("value")) %>%
      dplyr::mutate(account = as.numeric(stringr::str_replace_all(account, "612","400"))) %>%
      dplyr::select(company, account, commission)
    
    tmp3 <- competition[[i]]$capacity %>%
      dplyr::filter(purpose == "advertising") %>%
      dplyr::mutate(origin = as.numeric(stringr::str_replace_all(origin, "140","400"))) %>%
      dplyr::filter(origin %in% tmp1$account) %>%
      dplyr::select(company, account = origin, parameter, value) %>%
      tidyr::pivot_wider(names_from = c("parameter"), values_from = c("value")) %>%
      dplyr::mutate(advertising = quantity * price) %>%
      dplyr::select(company, account, advertising)
    
    preptmp4 <- competition[[i]]$capacity %>%
      dplyr::filter(parameter == "price", destination >= 13300, destination < 14000) %>%
      dplyr::select(company, account = destination, value)
    
    tmp4 <- competition[[i]]$technology %>%
      dplyr::filter(purpose == "production", input >= 13300, input < 14000) %>%
      dplyr::select(company, account = input, input_standard_quantity, output, output_standard_quantity) %>%
      dplyr::left_join(preptmp4, by = c("company","account")) %>%
      dplyr::mutate(cost = input_standard_quantity * value / output_standard_quantity) %>%
      dplyr::group_by(company, output) %>%
      dplyr::summarise(cost = sum(cost)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(account = as.numeric(stringr::str_replace_all(output, "910","400"))) %>%
      dplyr::select(company, account, cost)
    
    profiles[[i]] <- tmp1 %>%
      dplyr::left_join(tmp2, by = c("company","account")) %>%
      dplyr::left_join(tmp3, by = c("company","account")) %>%
      dplyr::left_join(tmp4, by = c("company","account")) %>%
      dplyr::select(company, account, everything())
    
    rm(tmp1,tmp2,tmp3,preptmp4,tmp4)
  }
  
  profiles <- dplyr::bind_rows(profiles) %>%
    dplyr::mutate(period = simperiod) %>%
    dplyr::select(company, period, dplyr::everything())
  
  
  
  ###########################################################################################################################
  
  zero2one <- function(x) (x-min(x))/(max(x)-min(x))
  
  attractiveness <- profiles %>%
    dplyr::mutate(price = price * (1- discount)) %>%
    dplyr::mutate(
      dso = zero2one(dso),
      price = zero2one(price),
      commission = zero2one(commission),
      advertising = zero2one(advertising),
      cost = zero2one(cost)
    ) %>%
    tidyr::replace_na(list(price = 0.5, dso = 0.5, commission = 0.5, advertising = 0.5, cost = 0.5)) %>%
    dplyr::mutate(sensitivity = list(dplyr::select(base_market$environments, dplyr::starts_with("sensitivity_")))) %>%
    tidyr::unnest(sensitivity) %>%
    dplyr::mutate(attractiveness = 100 * (
      dso * sensitivity_dso +
        advertising * sensitivity_advertising +
        commission * sensitivity_commissions +
        cost * sensitivity_quality -
        price * sensitivity_price
    ) / (sensitivity_price + sensitivity_dso + sensitivity_advertising + sensitivity_commissions + sensitivity_quality)
    ) %>%
    dplyr::select(company, account, attractiveness)
  
  shape <- base_market$environments$sensitivity_shape[[1]]
  sensitivity <- base_market$environments$sensitivity_demand[[1]]
  
  linear <- function(p, alpha, beta) alpha*p + beta
  constant <- function(p, alpha, beta) exp(alpha*log(p)+beta)
  logistic <- function(p, c, alpha, p0) c/(1+exp(-alpha*(p-p0)))
  
  coefficient <- c()
  
  for (i in attractiveness$attractiveness){
    j <- switch(
      shape,
      linear = linear((100-i), -1, 100),
      constant = constant((100-i), -0.3, 4.75),
      logistic = logistic((100-i), 100, -0.2, 50)
    )
    
    coefficient <- c(coefficient, j)
  }
  
  coefficient <- (coefficient - min(coefficient)) / (max(coefficient) - min(coefficient))
  coefficient <- coefficient / sum(coefficient)
  coefficient <- ((1-sensitivity) + sensitivity * coefficient) / length(coefficient)
  
  attractiveness$demand <- round(simdemand * length(profiles) * coefficient, 0)
  
  
  
  ###########################################################################################################################
  
  
  profiles <- profiles %>%
    dplyr::left_join(attractiveness, by = c("company","account"))
  
  profiles <- split(profiles, profiles$company)
  
  for (i in 1:length(competition)){
    
    company <- names(competition)[i]
    competition[[company]]$profile <- unique(dplyr::filter(dplyr::bind_rows(competition[[company]]$profile, profiles[[company]]), period != ""))
    
  }
  
  return(competition)
}

