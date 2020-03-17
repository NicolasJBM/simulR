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
  
  capacity <- simulR::case_capacity %>%
    dplyr::filter(case == company$case)
  
  activity <- simulR::case_activity %>%
    dplyr::filter(case == company$case)
  
  costing <- simulR::case_costing %>%
    dplyr::filter(case == company$case)
  
 
  ###################################################################################################
  
  assets_entries <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(contract == "investment") %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::mutate(asset_id = purrr::map(quantity, function(x) 1:x)) %>%
    dplyr::mutate(quantity = 1) %>%
    tidyr::unnest(asset_id) %>%
    dplyr::mutate(
      date = start_date,
      rate = company$interest_rate,
      resource = paste0(resource, " - ", asset_id)
    ) %>%
    dplyr::select(date, object = resource, price, rate, lifetime, nature, destination) %>%
    purrr::pmap(simulR::record_assets) %>%
    dplyr::bind_rows()
  
  
  prepaid_entries <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(contract == "purchase") %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::filter(quantity >= 1) %>%
    dplyr::select(object = resource, quantity, price, dpo, discount, lifetime, nature, destination) %>%
    dplyr::mutate(
      date = start_date, vat = environment$value_added_tax, risk = company$risk_purchaser
    ) %>%
    dplyr::select(date, object, quantity, price, discount, vat, dpo, risk, lifetime, nature, destination) %>%
    purrr::pmap(simulR::record_purchases) %>%
    dplyr::bind_rows()
  
  
  discount = 0.05
  vat = 0.2
  dpo = 45
  account_increment = 0
  risk = 0.1
  
  ###################################################################################################
  
  need_financing <- assets_entries %>% 
    left_join(simulR::case_accounts, by = "account") %>%
    filter(date == start_date, account_generic == "cash") %>%
    select(credit) %>%
    sum()
  
  cash <- ceiling(need_financing * company$cash_buffer)
  debt <- ceiling((need_financing+cash) * company$debt_share)
  equity <- need_financing+cash-debt
  
  
  
  
  
  
  
  
  
  
  
  
  
  return(results)
}
