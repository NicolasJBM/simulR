#' Functions generating cases for exercises
#' @param case                  Character vector. Names of the case to select.
#' @param number_cost_objects     Integer. Number of different produts sold.
#' @param number_cost_pools       Integer. Number of different cost pools used.
#' @param number_materials        Integer. Number of different materials used.
#' @param number_production_steps Integer. Number of sequential production steps.
#' @param number_joint_products   Integer. Number of products which are joint.
#' @param start_date              Date. At which date the first balance sheet is initialized.
#' @importFrom dplyr %>%
#' @importFrom dplyr filter 
#' @importFrom dplyr sample_n
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr arrange
#' @importFrom purrr pmap
#' @importFrom tidyr pivot_wider 
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr unnest
#' @return list of sublists necessary information for simulation and documentation:
#' \item{base_market}{parameters about the environment, seasonality, chart of accounts and periods in the case.}
#' \item{company}{parameters about the company, its capacity, activity, costing system, inventories and beginning of journal.}
#' @export


create_case <- function(case = NA,
                        number_cost_objects = 3,
                        number_cost_pools = 3,
                        number_materials = 5,
                        number_production_steps = 1,
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
  account_statement <- NULL
  account_subcategory <- NULL
  account <- NULL
  account_category <- NULL
  account_generic <- NULL
  account_label <- NULL
  asset_id <- NULL
  contract <- NULL
  credit <- NULL
  debit <- NULL
  destination <- NULL
  discount <- NULL
  dpo <- NULL
  first_period <- NULL
  initialization <- NULL
  keep <- NULL
  label <- NULL
  lifetime <- NULL
  map <- NULL
  nature <- NULL
  object <- NULL
  parameter <- NULL
  period <- NULL
  price <- NULL
  quantity <- NULL
  rate <- NULL
  resource <- NULL
  risk <- NULL
  unit <- NULL
  value <- NULL
  vat <- NULL
  
  
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
    if (is.na(number_production_steps)){
      company <- company
    } else {
      company <- dplyr::filter(company, number_production_steps == number_production_steps)
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
  
  environments <- simulR::case_environments %>%
    dplyr::filter(case == company$case) %>%
    dplyr::sample_n(1) %>%
    dplyr::select(-case)
  
  seasons <- simulR::case_seasons %>%
    dplyr::filter(case == company$case) %>%
    dplyr::select(-case)
  
  accounts <- simulR::case_accounts %>%
    dplyr::filter(case == company$case) %>%
    dplyr::select(-case)
  
  capacity <- simulR::case_capacity %>%
    dplyr::filter(case == company$case) %>%
    dplyr::select(-case)
  
  activity <- simulR::case_activity %>%
    dplyr::filter(case == company$case) %>%
    dplyr::select(-case)
  
  costing <- simulR::case_costing %>%
    dplyr::filter(case == company$case) %>%
    dplyr::select(-case)
  
 
  ###################################################################################################
  
  lta_entries <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(contract == "investment") %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::mutate(asset_id = purrr::map(quantity, function(x) 1:x)) %>%
    dplyr::mutate(quantity = 1) %>%
    tidyr::unnest(asset_id) %>%
    dplyr::mutate(
      date = start_date,
      resource = paste0(resource, " - ", asset_id)
    ) %>%
    dplyr::select(date, object = resource, price, rate, lifetime, nature, destination) %>%
    purrr::pmap(simulR::record_assets) %>%
    dplyr::bind_rows()
  
  assets_entries <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(contract == "purchase") %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::filter(quantity >= 1) %>%
    dplyr::select(object = resource, quantity, price, dpo, discount, lifetime, nature, destination) %>%
    dplyr::mutate(
      date = start_date, vat = environments$value_added_tax, risk = company$risk_purchaser
    ) %>%
    dplyr::select(date, object, quantity, price, discount, vat, dpo, risk, lifetime, nature, destination) %>%
    purrr::pmap(simulR::record_purchases) %>%
    dplyr::bind_rows() %>% 
    dplyr::bind_rows(lta_entries)
  
  rm(lta_entries)
  
  
  ###################################################################################################
  
  
  finance_entries <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(contract == "financing") %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::filter(quantity > 0) %>%
    dplyr::mutate(date = start_date) %>%
    dplyr::select(date, object = resource, quantity, price, rate, lifetime, nature, destination) %>%
    purrr::pmap(simulR::record_financing) %>%
    dplyr::bind_rows()
  
  
  ###################################################################################################
  
  journal <- assets_entries %>%
    dplyr::bind_rows(finance_entries) %>%
    dplyr::mutate(
      debit = round(debit, 2),
      credit = round(credit, 2)
    ) %>%
    dplyr::mutate(
      keep = dplyr::case_when(
        debit == 0 & is.na(credit) ~ FALSE,
        credit == 0 & is.na(debit) ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::filter(keep == TRUE) %>%
    dplyr::select(-keep) %>%
    dplyr::left_join(accounts, by = "account") %>%
    dplyr::select(date, label, account, account_label,
                  account_generic, account_subcategory, account_category, account_statement,
                  debit, credit)
  
  
  rm(assets_entries, finance_entries)
  
  ###################################################################################################
  
  inventory <- capacity %>%
    filter(parameter %in% c("quantity","price"), nature >= 13000 & nature < 14000 | nature >= 31000 & nature < 34000) %>%
    select(resource, parameter, initialization) %>%
    pivot_wider(names_from = c("parameter"), values_from = c("initialization")) %>%
    filter(quantity > 0) %>%
    mutate(date = start_date, value = quantity * price) %>%
    select(date, resource, quantity, price, value)
  
  
  ###################################################################################################
  
  
  periods <- journal$date %>%
    unique() %>%
    map(simulR::create_period) %>%
    dplyr::bind_rows()
  
  
  ###################################################################################################
  
  base_market <- list()
  base_market$environments <- environments
  base_market$seasons <- seasons
  base_market$accounts <- accounts
  base_market$periods <- periods
  
  rm(environments, seasons, accounts)
  
  base_company <- list()
  base_company$company <- company
  base_company$capacity <- capacity %>%
    dplyr::select(-initialization) %>%
    dplyr::rename(value = first_period)
  base_company$activity <- activity
  base_company$costing <- costing
  base_company$journal <- journal
  base_company$inventory <- inventory
  
  rm(company, capacity, activity, costing, journal)
  
  results <- list()
  results$base_market <- base_market
  results$base_company <- base_company
  
  return(results)
}
