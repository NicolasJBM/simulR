#' Functions generating cases for exercises
#' @param product       Character vector. Names of the product to select.
#' @param family        Character. Name of the family to select.
#' @param nbr_products  Integer. Number of different products.
#' @param mono_material Logical. Whether the selected products should be based on a unique material.
#' @param joint         Logical. Whether the selected products should be joint products.
#' @param nbr_companies Integer. Number of different companies.
#' @importFrom dplyr %>%
#' @importFrom dplyr filter 
#' @importFrom dplyr sample_n
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @return list of sublists necessary information for simulation and documentation:
#' \item{description}{Stable information about "families", "products", "activities", "resources", "contracts" and "consumptions.}
#' \item{environment}{Parameters controlled by the game master: "seasons", "parameters" and "contract_premia".}
#' \item{competition}{Parameters controlled or influenced by the players at the following levels: "company", "product", "resource" and "product_resource"}
#' @export



select_cases <- function(product = NULL,
                         family = NULL,
                         nbr_products = 1,
                         mono_material = FALSE,
                         joint = FALSE,
                         nbr_companies = 1){
  
  
  stopifnot(
    is.numeric(nbr_products),
    is.logical(mono_material),
    is.logical(joint),
    is.numeric(nbr_companies)
  )
  
  
  # Bind variables
  activity_description <- NULL
  activity_full <- NULL
  activity_order <- NULL
  activity_process <- NULL
  asset_disposal_loss <- NULL
  base_volume <- NULL
  cancellation_cost <- NULL
  case <- NULL
  coefficient <- NULL
  company <- NULL
  contract_behavior <- NULL
  contract_description <- NULL
  contract_driver <- NULL
  contract_full <- NULL
  contract_premium <- NULL
  cost_driver <- NULL
  cost_inventoriability <- NULL
  cost_of_capital <- NULL
  cost_traceability <- NULL
  cost_type <- NULL
  country <- NULL
  debt_share <- NULL
  debt_years <- NULL
  demand <- NULL
  desired_rate_of_return <- NULL
  dio <- NULL
  dividend <- NULL
  dpo <- NULL
  dso <- NULL
  family_description <- NULL
  family_full <- NULL
  family_plur <- NULL
  family_sing <- NULL
  income_tax <- NULL
  industry <- NULL
  interest_rate <- NULL
  labor_tax <- NULL
  loan <- NULL
  overdraft_rate <- NULL
  period <- NULL
  product_advertising <- NULL
  product_full <- NULL
  product_joint <- NULL
  product_minimum_order <- NULL
  product_mono_material <- NULL
  product_output <- NULL
  product_plur <- NULL
  product_price <- NULL
  product_sequence <- NULL
  product_sing <- NULL
  product_variety <- NULL
  resource_capacity <- NULL
  resource_consumed <- NULL
  resource_full <- NULL
  resource_plur <- NULL
  resource_price <- NULL
  resource_quantity <- NULL
  resource_sing <- NULL
  resource_type <- NULL
  resource_unit <- NULL
  sensitivity_advertising <- NULL
  sensitivity_dso <- NULL
  sensitivity_price <- NULL
  sensitivity_prime_cost <- NULL
  severance_cost <- NULL
  value_added_tax <- NULL
  
  
  
  # Select
  if (!is.null(product)){
    base <- dplyr::filter(simulR::case_products, product_full == product)
  } else if (!is.null(family)){
    base <- dplyr::filter(simulR::case_products, family_full == family)
  } else {
    base <- simulR::case_products
  }
  
  base <- dplyr::filter(base, product_variety >= nbr_products, product_joint == joint)
  if (mono_material == TRUE) base <- dplyr::filter(base, product_mono_material == TRUE)
  
  slct_case <- sample(unique(base$case),1)
  base <- dplyr::filter(base, case == slct_case)
  
  slct_family <- sample(unique(base$family_full),1)
  base <- dplyr::filter(base, family_full == slct_family)
  
  slct_product <- sample(unique(base$product_full), nbr_products)
  base <- dplyr::filter(base, product_full %in% slct_product) %>%
    dplyr::left_join(simulR::case_families, by = c("case","family_full"))
  
  
  seasons <- filter(simulR::case_seasons, case == slct_case) %>%
    dplyr::select(-case) %>%
    tidyr::pivot_longer(cols = c(2:8), names_to = "weekday", values_to = "coefficient") %>%
    dplyr::mutate(coefficient = coefficient/max(coefficient))
  
  
  # Gather
  base <- base %>%
    dplyr::left_join(simulR::case_consumptions, by = c("case","product_full")) %>%
    dplyr::left_join(simulR::case_contracts, by = c("case","contract_full")) %>%
    dplyr::left_join(simulR::case_resources, by = c("case","resource_full")) %>%
    dplyr::left_join(simulR::case_activities, by = c("case","activity_full")) %>%
    dplyr::ungroup()
  
  
  companies <- dplyr::filter(simulR::case_industries, case == slct_case, industry == unique(base$industry)) %>%
    dplyr::sample_n(nbr_companies) %>%
    dplyr::select(-case) %>%
    dplyr::mutate(period = NA)
  
  
  
  
  # Split
  description <- list()
  description$families <- base %>%
    dplyr::select(
      case,
      industry,
      family_full,
      family_sing,
      family_plur,
      family_description
    ) %>%
    unique()
  description$products <- base %>%
    dplyr::select(
      product_full,
      product_sing,
      product_plur,
      product_variety,
      product_mono_material,
      product_sequence,
      product_joint
    ) %>%
    unique()
  description$activities <- base %>%
    dplyr::select(
      activity_full,
      activity_order,
      activity_process,
      activity_description
    ) %>%
    unique()
  description$resources <- base %>%
    select(
      resource_full,
      resource_sing,
      resource_plur,
      resource_type,
      resource_unit
    ) %>%
    unique()
  description$contracts <- base %>%
    dplyr::select(
      contract_full,
      contract_behavior,
      contract_driver,
      contract_description
    ) %>%
    unique()
  description$consumptions <- base %>%
    dplyr::select(
      case,
      product_full,
      activity_full,
      resource_full,
      cost_inventoriability,
      cost_traceability,
      cost_type,
      cost_driver,
      product_output
  )
  
  
  
  environment <- list()
  environment$seasons <- seasons
  environment$parameters <- base %>%
    dplyr::mutate(period = NA) %>%
    dplyr::select(
      period,
      base_volume, 
      sensitivity_prime_cost,
      sensitivity_advertising,
      sensitivity_price,
      sensitivity_dso,
      value_added_tax,
      labor_tax,
      income_tax,
      severance_cost,
      cancellation_cost,
      asset_disposal_loss
    ) %>%
    unique()
  environment$contract_premia <- base %>%
    dplyr::mutate(period = NA) %>%
    dplyr::select(period, contract_full, contract_premium)
  
  
  
  
  competition <- list()
  competition$company <- companies %>%
    dplyr::mutate(period = 0, loan = 0, dividend = 0) %>%
    dplyr::select(company, period, country, desired_rate_of_return, interest_rate, overdraft_rate, cost_of_capital, debt_share, debt_years, dso, dio, dpo, loan, dividend)
  competition$product <- list()
  competition$resource <- list()
  competition$product_resource <- list()
  
  for (i in 1:nrow(companies)){
    
    tmp1 <- base %>%
      dplyr::mutate(
        company = companies$company[[i]],
        period = NA,
        loan = 0,
        dividend = 0
      )
    
    competition$product[[i]] <- unique(dplyr::select(tmp1,company,period,product_full,product_price,product_advertising,product_minimum_order))
    competition$resource[[i]] <- unique(dplyr::select(tmp1,company,period,resource_full,contract_full,resource_quantity,resource_capacity,resource_price))
    competition$product_resource[[i]] <- unique(dplyr::select(tmp1,company,period,product_full,activity_full,resource_full,resource_consumed))
  }
    
  competition$product <- dplyr::bind_rows(competition$product)
  competition$resource <- dplyr::bind_rows(competition$resource)
  competition$product_resource <- dplyr::bind_rows(competition$product_resource)
 
  results <- list(
    description = description,
    environment = environment,
    competition = competition
  )
  
  return(results)
}
