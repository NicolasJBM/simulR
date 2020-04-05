#' Create a base market and a base profile for companies based on case selection criteria.
#' @param case             List. Tibbles about case information, environments, seasons, accounts, capacity, technology and costing.
#' @param start_date       Date. At which date the first balance sheet is initialized.
#' @param number_months    Integer. How many months should the time series last.
#' @param number_companies Integer. How many companies should be created.
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
#' @importFrom lubridate is.Date
#' @importFrom lubridate day
#' @importFrom lubridate days_in_month
#' @importFrom tidyr pivot_wider 
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr unnest
#' @return list of sublists necessary information for simulation and documentation:
#' \item{base_market}{parameters about the environment, chart of accounts and market in the case.}
#' \item{base_company}{parameters about the company, its capacity, technology, beginning of journal and censuses of various resources.}
#' @export


create_case <- function(case = NULL,
                        start_date = Sys.Date(),
                        number_months = 12,
                        number_companies = 1){
  
  
  stopifnot(
    !is.null(case),
    lubridate::is.Date(start_date),
    is.numeric(number_months),
    is.numeric(number_companies),
    number_companies <= 9
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
  actual <- NULL
  forecast <- NULL
  working_day <- NULL
  
  
  
  lubridate::day(start_date) <- lubridate::days_in_month(start_date)
  
  
  ##################################################################################################
  # Select the case based on filters and create a market
  information <- case$information
  environments <- case$environments
  seasons <- case$seasons %>%
    tidyr::pivot_longer(cols = c("monday","tuesday","wednesday","thursday","friday","saturday","sunday"),
                        names_to = "weekday", values_to = "coefficient")
  accounts <- case$accounts
  capacity <- case$capacity
  technology <- case$technology
  base_costing <- case$costing
  
  market <- simulR::create_market(start = start_date,
                                  number_months = number_months,
                                  base_volume = environments$base_volume,
                                  seasons = seasons) %>%
    dplyr::mutate(date = purrr::map(date, simulR::create_period)) %>%
    tidyr::unnest(date)
  
  
  periodic_demand <- market %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(working_day = sum(working_day), forecast = sum(forecast), actual = sum(actual))
  periodic_demand <- periodic_demand[-1,]
  
  
  rm(seasons)
  
  
  company_names <- information %>%
    dplyr::select(company_names) %>%
    unlist() %>%
    strsplit("; ") %>%
    unlist() %>%
    sample(number_companies, replace = FALSE) %>%
    as.character()
  
  
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
    dplyr::select(date, quantity, price, rate, duration, origin) %>%
    purrr::pmap(simulR::record_debt) %>%
    dplyr::bind_rows()
  
  
  
  equity <- capacity %>%
    dplyr::select(-first_period, -unit) %>%
    dplyr::filter(nature == "equity") %>%
    tidyr::pivot_wider(names_from = c(parameter), values_from = c(initialization)) %>%
    dplyr::filter(origin < 39000)
  
  equity_entries <- equity %>%
    dplyr::mutate(date = start_date) %>%
    dplyr::select(date, quantity, price, par, origin, destination) %>%
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
  
  costing$services_distribution <- tibble::tibble(period = "", from_pool = NA, from = "",
                                                  to_pool = NA, to = "", quantity = NA,
                                                  type = "", total_from = NA, proportion_from = NA, rank_from = NA, rank_to = NA)
  
  costing$allocation_rates <- tibble::tibble(period = "", costing = "", method = "", cost_pool = NA, accumulated = NA, allocated = NA, allocation = NA, allocation_base = NA, allocation_rate = NA)
  costing$assignment_table <- tibble::tibble(period = "", step = "", from = NA)
  
  
  
  ###################################################################################################
  
  base_market <- list()
  base_market$environments <- environments
  base_market$accounts <- accounts
  base_market$market <- market
  base_market$periodic_demand <- periodic_demand
  
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
    attractiveness = 0, demand = 0, sales = 0)
  base_company$activity <- tibble::tibble(company = "", period = "", purpose = "", input = 0, output = 0, quantity = 0, unit = "")
  base_company$usage <- tibble::tibble(company = "", period = "", account = 0, beginning = 0, purchased = 0, consumed = 0, produced = 0, sold = 0, unused = 0)
  base_company$census <- census
  base_company$costing <- costing
  
  rm(capacity, technology, journal, census)
  
  competition <- simulR::create_competition(company_names = company_names, base_company = base_company)
  
  results <- list()
  results$base_market <- base_market
  results$competition <- competition
  
  
  return(results)
}
