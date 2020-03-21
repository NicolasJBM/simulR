#' Update information about companies' competitive profiles in competition. 
#' @param simperiod   Character. ID of the period for which the profile holds.
#' @param simdemand      Integer. Demand for the period.
#' @param competition List. competitors as returned by the function make_competition.
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


update_profiles <- function(simperiod, simdemand, competition, base_market){
  
  
  # Bind variables
  advertising <- NULL 
  commission <- NULL 
  contract <- NULL 
  cost <- NULL 
  discount <- NULL 
  dso <- NULL
  input <- NULL
  input_standard_quantity <- NULL
  nature <- NULL
  output <- NULL
  output_standard_quantity <- NULL
  parameter <- NULL
  period <- NULL
  phase <- NULL
  price <- NULL
  quantity <- NULL
  resource <- NULL
  resource_type <- NULL
  sensitivity <- NULL
  sensitivity_advertising <- NULL
  sensitivity_commissions <- NULL
  sensitivity_dso <- NULL
  sensitivity_price <- NULL
  sensitivity_quality <- NULL
  value <- NULL
  
  
  profiles <- list()
  
  for (i in 1:length(competition)){
    
    tmp1 <- competition[[i]]$capacity %>%
      dplyr::filter(contract == "sale", nature >= 40000) %>%
      dplyr::select(company, resource, parameter, value) %>%
      tidyr::pivot_wider(names_from = c("parameter"), values_from = c("value"))
    
    tmp2 <- competition[[i]]$capacity %>%
      dplyr::filter(contract == "purchase", resource %in% tmp1$resource) %>%
      dplyr::select(company, resource, parameter, value) %>%
      tidyr::pivot_wider(names_from = c("parameter"), values_from = c("value")) %>%
      dplyr::mutate(advertising = quantity * price) %>%
      dplyr::select(company, resource, advertising)
    
    tmp3 <- competition[[i]]$technology %>%
      dplyr::left_join(rename(base_market$resources, input = resource), by = "input") %>%
      dplyr::filter(phase == "production", resource_type == "materials") %>%
      dplyr::select(company, output, resource = input, output_standard_quantity, input_standard_quantity) %>%
      dplyr::left_join(dplyr::filter(competition[[i]]$capacity, parameter == "price"), by = c("company","resource")) %>%
      dplyr::mutate(cost = input_standard_quantity * value / output_standard_quantity) %>%
      dplyr::group_by(company, output) %>%
      dplyr::summarise(cost = sum(cost)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(resource = output)
    
    profiles[[i]] <- tmp1 %>%
      dplyr::left_join(tmp2, by = c("company","resource")) %>%
      dplyr::left_join(tmp3, by = c("company","resource"))
    
    rm(tmp1,tmp2,tmp3)
  }
  
  profiles <- dplyr::bind_rows(profiles) %>%
    dplyr::mutate(period = simperiod) %>%
    dplyr::select(period, dplyr::everything(), -quantity)
  
  
  
  ###########################################################################################################################
  
  attractiveness <- profiles %>%
    dplyr::mutate_if(is.numeric, function(x) (x-min(x))/(max(x)-min(x))) %>%
    tidyr::replace_na(list(price = 0.5, discount = 0.5, dso = 0.5, commission = 0.5, advertising = 0.5, cost = 0.5)) %>%
    dplyr::mutate(sensitivity = list(dplyr::select(base_market$environments, dplyr::starts_with("sensitivity_")))) %>%
    tidyr::unnest(sensitivity) %>%
    dplyr::mutate(attractiveness = 100 * (
      dso * sensitivity_dso +
        advertising * sensitivity_advertising / 2 +
        commission * sensitivity_commissions +
        cost * sensitivity_quality -
        price * (1-discount) * sensitivity_price
    ) / (sensitivity_price + sensitivity_dso + sensitivity_advertising + sensitivity_commissions + sensitivity_quality)
    ) %>%
    dplyr::select(company, resource, attractiveness)
  
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
    dplyr::left_join(attractiveness, by = c("company","resource"))
  
  profiles <- split(profiles, profiles$company)
  
  for (i in 1:length(competition)){
    
    company <- names(competition)[i]
    competition[[company]]$profile <- dplyr::filter(dplyr::bind_rows(competition[[company]]$profile, profiles[[company]]), period != "")
    
  }
  
  return(competition)
}

