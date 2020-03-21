#' Create a base market and a base profile for companies based on case selection criteria.
#' @param case                  Character vector. Names of the case to select.
#' @param number_cost_objects     Integer. Number of different produts sold.
#' @param number_cost_pools       Integer. Number of different cost pools used.
#' @param number_materials        Integer. Number of different materials used.
#' @param number_joint_products   Integer. Number of products which are joint.
#' @param start_date              Date. At which date the first balance sheet is initialized.
#' @param number_years            Integer. How many years should the time series last.
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
#' @importFrom purrr map
#' @importFrom tidyr pivot_wider 
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr unnest
#' @return list of sublists necessary information for simulation and documentation:
#' \item{base_market}{parameters about the environment, chart of accounts, resources and market in the case.}
#' \item{base_company}{parameters about the company, its capacity, technology and underlying costing system, beginning of journal and censuses of various resources.}
#' @export


create_case <- function(case = NA,
                        number_cost_objects = 3,
                        number_cost_pools = 3,
                        number_materials = 5,
                        number_joint_products = 0,
                        start_date = Sys.Date(),
                        number_years = 5){
  
  
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
    dplyr::select(-case) %>%
    tidyr::pivot_longer(cols = c("monday","tuesday","wednesday","thursday","friday","saturday","sunday"),
                        names_to = "weekday", values_to = "coefficient")
  
  accounts <- simulR::case_accounts %>%
    dplyr::filter(case == company$case) %>%
    dplyr::select(-case)
  
  resources <- simulR::case_resources %>%
    dplyr::filter(case == company$case) %>%
    dplyr::select(-case)
  
  capacity <- simulR::case_capacity %>%
    dplyr::filter(case == company$case) %>%
    dplyr::select(-case)
  
  technology <- simulR::case_technology %>%
    dplyr::filter(case == company$case) %>%
    dplyr::select(-case)
  
  market <- simulR::create_market(start = start_date,
                                  years = number_years,
                                  base_volume = environments$base_volume,
                                  seasons = seasons) %>%
    dplyr::mutate(date = purrr::map(date, simulR::create_period)) %>%
    tidyr::unnest(date)
  
  rm(seasons)
  
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
    purrr::pmap(simulR::record_investment) %>%
    dplyr::bind_rows()
  
  assets_entries <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(contract == "purchase") %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::filter(quantity >= 1) %>%
    dplyr::select(object = resource, quantity, price, dpo, discount, lifetime, risk, nature, destination) %>%
    dplyr::mutate(
      date = start_date, vat = environments$value_added_tax
    ) %>%
    dplyr::select(date, object, quantity, price, discount, vat, dpo, risk, lifetime, nature, destination) %>%
    purrr::pmap(simulR::record_purchase) %>%
    dplyr::bind_rows() %>% 
    dplyr::bind_rows(lta_entries)
  
  rm(lta_entries)
  
  
  ###################################################################################################
  
  
  finance_entries <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(contract == "finance") %>%
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
                  debit, credit) %>%
    dplyr::filter(date %in% market$date)
  
  
  rm(assets_entries, finance_entries)
  
  ###################################################################################################
  
  
  census <- list()
  
  census$finished_products <- capacity %>%
    dplyr::filter(nature >= 13100, nature < 13200) %>%
    dplyr::select(resource, parameter, initialization) %>%
    tidyr::pivot_wider(names_from = c("parameter"), values_from = c("initialization")) %>%
    dplyr::mutate(date = start_date, value = quantity * price) %>%
    dplyr::select(date, resource, quantity, value)
  
  census$raw_materials <- capacity %>%
    dplyr::filter(destination >= 13300, destination < 13400) %>%
    dplyr::select(resource, parameter, initialization) %>%
    tidyr::pivot_wider(names_from = c("parameter"), values_from = c("initialization")) %>%
    dplyr::mutate(date = start_date, value = quantity * price) %>%
    dplyr::select(date, resource, quantity, value)
  
  census$assets <- capacity %>%
    dplyr::filter(nature >= 15000, nature < 16000) %>%
    dplyr::select(resource, parameter, initialization) %>%
    tidyr::pivot_wider(names_from = c("parameter"), values_from = c("initialization")) %>%
    dplyr::mutate(date = start_date, capacity = quantity * capacity) %>%
    dplyr::select(date, resource, capacity)
    
  census$equity <- capacity %>%
    dplyr::filter(nature >= 31000, nature < 39000) %>%
    dplyr::select(resource, parameter, initialization) %>%
    tidyr::pivot_wider(names_from = c("parameter"), values_from = c("initialization")) %>%
    dplyr::mutate(date = start_date, value = quantity * price) %>%
    dplyr::select(date, resource, quantity, value)
  
  
  ###################################################################################################
  
  base_market <- list()
  base_market$environments <- environments
  base_market$accounts <- accounts
  base_market$resources <- resources
  base_market$market <- market
  
  rm(environments, accounts)
  
  base_company <- list()
  base_company$company <- company
  base_company$capacity <- capacity %>%
    dplyr::select(-initialization) %>%
    dplyr::rename(value = first_period)
  base_company$technology <- technology
  base_company$journal <- journal
  base_company$profile <- tibble::tibble(period = "", resource = "", dso = 0, price = 0, discount = 0, commission = 0, advertising = 0, cost = 0, attractiveness = 0, demand = 0)
  base_company$activity <- tibble::tibble(period = "", resource = "", activity = "", quantity = 0, price = 0)
  base_company$census <- census
  
  rm(company, capacity, technology, journal, census)
  
  results <- list()
  results$base_market <- base_market
  results$base_company <- base_company
  
  return(results)
}
