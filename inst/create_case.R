#' Functions generating cases for exercises
#' @param case                  Character vector. Names of the case to select.
#' @param number_cost_objects   Integer. Number of different produts sold.
#' @param number_cost_pools     Integer. Number of different cost pools used.
#' @param number_materials      Integer. Number of different materials used.
#' @param number_joint_products Integer. Number of products which are joint.
#' @param start_date            Date. At which date the first balance sheet is initialized.
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



#'   \item 
#'   \item 
#'   \item 
#'   \item 

create_case <- function(case = NA,
                        number_cost_objects = 3,
                        number_cost_pools = 3,
                        number_materials = 5,
                        number_joint_products = 0,
                        start_date = Sys.Date()){
  
  
  stopifnot(
    is.na(case) | case %in% simulR::case_companies$case,
    is.na(number_cost_objects) | is.numeric(number_cost_objects),
    is.na(number_cost_pools) | is.numeric(number_cost_pools),
    is.na(number_materials) | is.numeric(number_materials),
    is.na(number_joint_products) | is.numeric(number_joint_products)
  )
  
  # Bind variables
  xxx <- NULL
  
  
  # Select
  if (is.na(case)){
    company <- simulR::case_companies
    if (is.na(number_cost_objects)){
      company <- company
    } else {
      company <- dplyr::filter(company, number_cost_objects == number_cost_objects)
    }
    if (is.na(number_cost_pools)){
      company <- company
    } else {
      company <- dplyr::filter(company, number_cost_pools == number_cost_pools)
    }
    if (is.na(number_materials)){
      company <- company
    } else {
      company <- dplyr::filter(company, number_materials == number_materials)
    }
    if (is.na(number_joint_products)){
      company <- company
    } else {
      company <- dplyr::filter(company, number_joint_products == number_joint_products)
    }
  } else {
    company <- dplyr::filter(simulR::case_companies, company == company)
  }
  
  company <- dplyr::sample_n(company,1) %>%
    as.data.frame() %>%
    as.vector()
  
  environment <- simulR::case_environments %>%
    dplyr::filter(case == company$case) %>%
    dplyr::sample_n(1)
  
  seasons <- simulR::case_seasons %>%
    dplyr::filter(case == company$case)
  
  accounts <- simulR::case_accounts %>%
    dplyr::filter(case == company$case)
  
  resources <- simulR::case_resources %>%
    dplyr::filter(case == company$case)
  
  sales <- simulR::case_sales %>%
    dplyr::filter(case == company$case) %>%
    dplyr::sample_n(1)
  
  capacity <- simulR::case_capacity %>%
    dplyr::filter(case == company$case)
  
  activity <- simulR::case_activity %>%
    dplyr::filter(case == company$case)
  
  costing <- simulR::case_costing %>%
    dplyr::filter(case == company$case)
  
 
  
  assets <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(contract %in% c("investment","lease")) %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::mutate(asset_id = purrr::map(quantity, function(x) 1:x)) %>%
    dplyr::mutate(quantity = 1) %>%
    tidyr::unnest(asset_id) %>%
    dplyr::mutate(
      date = start_date,
      rate = dplyr::case_when(
        contract == "lease" ~ company$interest_rate,
        TRUE ~ NA
      ),
      resource = paste0(resource, " - ", asset_id)
    ) %>%
    dplyr::select(date, object = resource, price = value, rate, lifetime, nature, destination)
  
  
  
  
  
  
  
  
  
  
  
  
  base <- dplyr::filter(base, product_variety == nbr_products, product_joint == joint)
  if (mono_material == TRUE) base <- dplyr::filter(base, product_mono_material == TRUE)
  
  slct_case <- sample(unique(base$case),1)
  base <- dplyr::filter(base, case == slct_case)
  
  slct_family <- sample(unique(base$family_full),1)
  base <- dplyr::filter(base, family_full == slct_family) %>%
    dplyr::left_join(simulR::case_families, by = c("case","family_full"))
  
  
  seasons <- filter(simulR::case_seasons, case == slct_case) %>%
    dplyr::select(-case) %>%
    tidyr::pivot_longer(cols = c(2:8), names_to = "weekday", values_to = "coefficient") %>%
    dplyr::mutate(coefficient = coefficient/max(coefficient))
  
  
  # Gather
  base <- base %>%
    dplyr::left_join(simulR::case_consumptions, by = c("case","product_full")) %>%
    dplyr::left_join(simulR::case_resources, by = c("case","resource_full")) %>%
    dplyr::left_join(simulR::case_contracts, by = c("case","contract_full")) %>%
    dplyr::left_join(simulR::case_activities, by = c("case","activity_full")) %>%
    dplyr::ungroup()
  
  
  companies <- dplyr::filter(simulR::case_companies, case == slct_case) %>%
    dplyr::sample_n(nbr_companies) %>%
    dplyr::select(-case) %>%
    dplyr::mutate(period = NA)
  
  
  
  
  # Split
  description <- list()
  description$families <- base %>%
    dplyr::select(
      case,
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
      resource_name,
      resource_unit
    ) %>%
    unique()
  description$contracts <- base %>%
    dplyr::select(
      contract_full,
      contract_behavior,
      contract_driver,
      contract_description,
      contract_premium
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
      cost_driver
  )
  
  
  
  environment <- list()
  environment$seasons <- seasons
  environment$parameters <- base %>%
    dplyr::mutate(period = NA) %>%
    dplyr::select(
      period,
      base_volume, 
      sensitivity_prime_cost,
      sensitivity_commercial,
      sensitivity_price,
      sensitivity_dso,
      sensitivity_demand,
      severance_cost,
      asset_disposal_loss
    ) %>%
    unique()
  
  
  
  
  competition <- list()
  competition$company <- companies %>%
    dplyr::mutate(period = 0, loan = 0, dividend = 0) %>%
    dplyr::select(
      company,
      period,
      industry,
      country,
      desired_rate_of_return,
      interest_rate,
      overdraft_rate,
      cost_of_capital,
      debt_share,
      debt_years,
      income_tax,
      inventory_fp_policy,
      inventory_rm_policy,
      allowance_uncollectible,
      minimum_order,
      days_sales_outstanding,
      days_payable_outstanding,
      loan,
      dividend
  )
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
    
    competition$product[[i]] <- unique(dplyr::select(tmp1,
                                                     company,
                                                     period,product_full,
                                                     product_price,
                                                     product_priority,
                                                     product_commission))
    competition$resource[[i]] <- unique(dplyr::select(tmp1,
                                                      company,
                                                      period,
                                                      resource_full,
                                                      contract_full,
                                                      resource_years,
                                                      resource_tax,
                                                      resource_quantity,
                                                      resource_capacity,
                                                      resource_price,
                                                      resource_premium))
    competition$product_resource[[i]] <- unique(dplyr::select(tmp1,
                                                              company,
                                                              period,
                                                              product_full,
                                                              activity_full,
                                                              resource_full,
                                                              product_output,
                                                              resource_consumed))
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
