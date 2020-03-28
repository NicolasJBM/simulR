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
#' \item{base_market}{parameters about the environment, chart of accounts and market in the case.}
#' \item{base_company}{parameters about the company, its capacity, technology, beginning of journal and censuses of various resources.}
#' @export


create_case <- function(case = NA,
                        number_cost_objects = 3,
                        number_cost_pools = 3,
                        number_materials = 5,
                        number_joint_products = 0,
                        start_date = Sys.Date(),
                        number_years = 5){
  
  
  stopifnot(
    is.na(case) | case %in% simulR::case_information$case,
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
  nature <- NULL
  credit <- NULL
  debit <- NULL
  destination <- NULL
  destination_label <- NULL
  discount <- NULL
  dpo <- NULL
  first_period <- NULL
  initialization <- NULL
  keep <- NULL
  label <- NULL
  duration <- NULL
  map <- NULL
  origin <- NULL
  object <- NULL
  origin_label <- NULL
  par <- NULL
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
  
  
  
  ##################################################################################################
  # Select the case based on filters and create a market
  
  
  if (is.na(case)){
    case <- simulR::case_information
    if (is.na(number_cost_objects)){
      case <- case
    } else {
      case <- dplyr::filter(case, number_cost_objects == number_cost_objects)
    }
    if (is.na(number_cost_pools)){
      case <- case
    } else {
      case <- dplyr::filter(case, number_cost_pools == number_cost_pools)
    }
    if (is.na(number_materials)){
      case <- case
    } else {
      case <- dplyr::filter(case, number_materials == number_materials)
    }
    if (is.na(number_joint_products)){
      case <- case
    } else {
      case <- dplyr::filter(case, number_joint_products == number_joint_products)
    }
  } else {
    case <- dplyr::filter(simulR::case_information, case == case)
  }
  
  case <- sample(case$case, 1)
  
  environments <- simulR::case_environments %>%
    dplyr::filter(case == case) %>%
    dplyr::sample_n(1) %>%
    dplyr::select(-case)
  
  seasons <- simulR::case_seasons %>%
    dplyr::filter(case == case) %>%
    dplyr::select(-case) %>%
    tidyr::pivot_longer(cols = c("monday","tuesday","wednesday","thursday","friday","saturday","sunday"),
                        names_to = "weekday", values_to = "coefficient")
  
  accounts <- simulR::case_accounts %>%
    dplyr::filter(case == case) %>%
    dplyr::select(-case)
  
  capacity <- simulR::case_capacity %>%
    dplyr::filter(case == case) %>%
    dplyr::select(-case)
  
  technology <- simulR::case_technology %>%
    dplyr::filter(case == case) %>%
    dplyr::select(-case)
  
  base_costing <- simulR::case_costing %>%
    dplyr::filter(case == case) %>%
    dplyr::select(-case)
  
  market <- simulR::create_market(start = start_date,
                                  years = number_years,
                                  base_volume = environments$base_volume,
                                  seasons = seasons) %>%
    dplyr::mutate(date = purrr::map(date, simulR::create_period)) %>%
    tidyr::unnest(date)
  
  rm(seasons)
  
  
  ###################################################################################################
  
  
  materials <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(nature == "materials") %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::select(object = origin_label, quantity, price, dpo, discount, risk, origin, destination) %>%
    dplyr::mutate(
      date = start_date,
      vat = environments$value_added_tax,
      risk = case_when(is.na(risk) ~ 0, TRUE ~ risk),
      dpo = case_when(is.na(dpo) ~ 0, TRUE ~ dpo),
      discount = case_when(is.na(discount) ~ 0, TRUE ~ discount)
    )
  
  inv_entries <- materials %>%
    dplyr::select(date, object, quantity, price, discount, vat, dpo, risk, origin, destination) %>%
    purrr::pmap(simulR::record_purchase) %>%
    dplyr::bind_rows()
  
  materials <- materials %>%
    dplyr::select(account = destination, quantity, price) %>%
    dplyr::mutate(date = start_date, value = quantity * price) %>%
    dplyr::select(date, account, quantity, value)
  
  
  
  oca_entries <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(nature == "prepaid") %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::filter(quantity >= 1) %>%
    dplyr::select(object = origin_label, quantity, price, duration, origin, destination) %>%
    dplyr::mutate(
      date = start_date,
      vat = environments$value_added_tax,
      risk = 0,
      dpo = 0,
      discount = 0
    ) %>%
    dplyr::select(date, object, quantity, price, discount, vat, dpo, risk, duration, origin, destination) %>%
    purrr::pmap(simulR::record_purchase) %>%
    dplyr::bind_rows()
  
  
  
  assets <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(nature == "investment") %>%
    tidyr::pivot_wider(names_from = c("parameter"), values_from = c("initialization"))
  
  lta_entries <- assets %>%
    dplyr::mutate(asset_id = purrr::map(quantity, function(x) 1:x)) %>%
    dplyr::mutate(quantity = 1) %>%
    tidyr::unnest(asset_id) %>%
    dplyr::mutate(
      date = start_date,
      object = paste0(origin_label, " - ", asset_id)
    ) %>%
    dplyr::select(date, object, price, rate, duration, origin, destination) %>%
    purrr::pmap(simulR::record_investment) %>%
    dplyr::bind_rows()
  
  assets <- assets %>%
    dplyr::mutate(date = start_date,
                  account = as.numeric(stringr::str_replace_all(origin, "^15", "16")),
                  capacity = quantity * capacity) %>%
    dplyr::select(date, account, capacity)
  
  
  
  debt_entries <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(nature == "debt") %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::filter(quantity > 0) %>%
    dplyr::mutate(date = start_date) %>%
    dplyr::select(date, object = origin_label, quantity, price, rate, duration, origin) %>%
    purrr::pmap(simulR::record_debt) %>%
    dplyr::bind_rows()
  
  
  
  equity <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(nature == "equity") %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::filter(origin < 39000)
  
  equity_entries <- equity %>%
    dplyr::mutate(date = start_date) %>%
    dplyr::select(date, object = origin_label, quantity, price, par, origin, destination) %>%
    purrr::pmap(simulR::record_equity) %>%
    dplyr::bind_rows()
  
  equity <- equity %>%
    dplyr::mutate(date = start_date) %>%
    dplyr::select(date, account = origin, par, quantity)
  
  
  
  journal <- inv_entries %>% 
    dplyr::bind_rows(oca_entries) %>% 
    dplyr::bind_rows(lta_entries) %>% 
    dplyr::bind_rows(debt_entries) %>%
    dplyr::bind_rows(equity_entries) %>%
    dplyr::mutate(
      debit = round(debit, 2),
      credit = round(credit, 2)
    ) %>%
    dplyr::mutate(
      keep = dplyr::case_when(
        debit == 0 & is.na(credit) ~ FALSE,
        is.na(debit) & credit == 0 ~ FALSE,
        debit == 0 & credit == 0 ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::filter(keep == TRUE) %>%
    dplyr::select(-keep) %>%
    dplyr::select(date, label, account, debit, credit) %>%
    dplyr::filter(date %in% market$date)
  
  rm(inv_entries, oca_entries, lta_entries, debt_entries, equity_entries)
  
  
  
  ###################################################################################################
  
  
  census <- list()
  
  census$finished_products <- capacity %>%
    dplyr::filter(destination >= 13100, destination < 13200) %>%
    dplyr::select(account = destination, parameter, initialization) %>%
    tidyr::pivot_wider(names_from = c("parameter"), values_from = c("initialization")) %>%
    dplyr::mutate(date = start_date, value = quantity * price) %>%
    dplyr::select(date, account, quantity, value)
  
  census$raw_materials <- materials
  
  census$assets <- assets
    
  census$people <- capacity %>%
    dplyr::filter(nature == "employment") %>%
    dplyr::select(account = origin, parameter, initialization) %>%
    tidyr::pivot_wider(names_from = c("parameter"), values_from = c("initialization")) %>%
    dplyr::mutate(date = start_date,
                  capacity = quantity * capacity) %>%
    dplyr::select(date, account, capacity)
  
  census$equity <- equity
  
  
  
  ###################################################################################################
  
  
  
  costing <- list()
  costing$base_costing <- base_costing
  
  costing$services_distribution <- tibble::tibble(company = "", period = "",
                                                  from_pool = NA, from = "",
                                                  to_pool = NA, to = "", quantity = NA,
                                                  type = "", total_from = NA, proportion_from = NA, rank_from = NA, rank_to = NA)
  
  costing$allocation_rates <- tibble::tibble(company = "", period = "", costing = "", method = "", cost_pool = NA, accumulated = NA, allocated = NA, allocation = NA, allocation_base = NA, allocation_rate = NA)
  costing$assignment_table <- tibble::tibble(company = "", period = "", step = "", from = NA)
  
  
  
  ###################################################################################################
  
  base_market <- list()
  base_market$environments <- environments
  base_market$accounts <- accounts
  base_market$market <- market
  
  rm(environments, accounts)
  
  base_company <- list()
  base_company$capacity <- capacity %>%
    dplyr::select(-initialization) %>%
    dplyr::rename(value = first_period)
  base_company$technology <- technology
  base_company$journal <- journal
  base_company$profile <- tibble::tibble(
    period = "", account = 0,
    dso = 0, price = 0, discount = 0, commission = 0, advertising = 0, cost = 0,
    attractiveness = 0, demand = 0)
  base_company$activity <- tibble::tibble(company = "", period = "", purpose = "", input = 0, output = 0, quantity = 0, unit = "")
  base_company$census <- census
  base_company$costing <- costing
  
  rm(capacity, technology, journal, census)
  
  results <- list()
  results$base_market <- base_market
  results$base_company <- base_company
  
  return(results)
}
