

make_period_profiles <- function(case_competition, case_environment, case_description){
  
  # Allocate the demand based on the market and companies' profiles
  tmp_price_adv_dso <- select(case$competition$product, -period) %>%
    left_join(select(case$competition$company, company, dso), by = "company") %>%
    select(company, product_full, price = product_price, advertising = product_advertising, dso)
  
  prep_costs <- case$competition$product_resource %>%
    left_join(case$competition$resource, by = c("company", "period", "resource_full")) %>%
    left_join(case$description$resources, by = "resource_full") %>%
    left_join(case$description$contract, by = "contract_full") %>%
    left_join(case$description$consumptions, by = c("product_full","activity_full","resource_full")) %>%
    left_join(case$description$activities, by = "activity_full") %>%
    mutate(cost = resource_price * resource_consumed / product_output) %>%
    select(company, product_full, contract_behavior, cost_inventoriability, cost_type, cost)
    
  value_added <- prep_costs %>%
    group_by(company, product_full, cost_type) %>%
    summarise(cost = sum(cost)) %>%
    pivot_wider(names_from = cost_type, values_from = cost)
    
  behavinv <- prep_costs %>%
    unite(behavinv, cost_inventoriability, contract_behavior, sep = "_") %>%
    group_by(company, product_full, behavinv) %>%
    summarise(cost = sum(cost)) %>%
    pivot_wider(names_from = behavinv, values_from = cost)
  
  
  profiles <- tmp_price_adv_dso %>%
    left_join(tmp_prime_cost, by = "reference") %>%
    left_join(tmp_unit_variable_cost, by = "reference") %>%
    left_join(tmp_unit_fixed_cost, by = "reference") %>%
    mutate(
      contribution_margin = price - unit_variable_cost,
      unit_gross_margin = price - unit_variable_cost - unit_fixed_cost
    )
  
  attractiveness <- profiles %>%
    simulR::simul_attractiveness(
      sensitivity_prime_cost = case$environment$parameters$sensitivity_prime_cost[[1]],
      sensitivity_advertising = case$environment$parameters$sensitivity_advertising[[1]],
      sensitivity_price = case$environment$parameters$sensitivity_price[[1]],
      sensitivity_dso = case$environment$parameters$sensitivity_dso[[1]]
    )
  
  profiles <- left_join(profiles, attractiveness, by = "reference")
  
  
  
  
  
  
  profiles$prime_cost <- sensitivity_prime_cost * ((profiles$prime_cost - min(profiles$prime_cost)) / (max(profiles$prime_cost) - min(profiles$prime_cost)))
  profiles$advertising <- sensitivity_advertising * ((profiles$advertising - min(profiles$advertising)) / (max(profiles$advertising) - min(profiles$advertising)))
  profiles$price <- sensitivity_price * (1 - ((profiles$price - min(profiles$price)) / (max(profiles$price) - min(profiles$price))))
  profiles$dso <- sensitivity_dso * ((profiles$dso - min(profiles$dso)) / (max(profiles$dso) - min(profiles$dso)))
  
  profiles$attractiveness <- (profiles$prime_cost + profiles$advertising + profiles$price + profiles$dso) / (sensitivity_prime_cost + sensitivity_advertising + sensitivity_price + sensitivity_dso)
  profiles$attractiveness <- round(profiles$attractiveness*100,0)
  
  
  profiles <- profiles[,c("reference","attractiveness")]
  
  
  
  
  
  
  return(profiles)
}
