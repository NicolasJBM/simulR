#' Simulate the specified period of activity.
#' @param case       List. output of either create_case of simulate_period.
#' @param period_nbr Integer. Number (i.d. order) of the period to simulate.
#' @return Append updated journal and census to the competitors' parameters.
#' @importFrom dplyr %>%
#' @return list of sublists necessary information for simulation and documentation:
#' \item{base_market}{parameters about the environment, chart of accounts and market in the case.}
#' \item{base_company}{parameters about the company, its capacity, technology, beginning of journal and censuses of various resources.}
#' @export


simulate_period <- function(case, period_nbr){
  
  base_market <- case$base_market
  simperiod <- base_market$periodic_demand$period[[period_nbr]]
  simdemand <- base_market$periodic_demand$actual[[period_nbr]]
  simworkdays <- base_market$periodic_demand$working_day[[period_nbr]]
  
  #case$competition
  
  case$competition <- case$competition %>%
    simulR::update_profiles(simperiod = simperiod,
                            simdemand = simdemand,
                            base_market = base_market) %>%
    simulR::update_activity(simperiod = simperiod,
                            simworkdays = simworkdays) %>%
    simulR::update_expenses(simperiod = simperiod,
                            simworkdays = simworkdays,
                            base_market = base_market) %>%
    simulR::update_production(simperiod = simperiod,
                              base_market = base_market) %>%
    simulR::update_sales(simperiod = simperiod,
                         base_market = base_market) %>%
    simulR::update_closing(simperiod = simperiod,
                           base_market = base_market)
  
  return(case)
}
