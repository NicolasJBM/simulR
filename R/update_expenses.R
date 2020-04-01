#' Update information about companies' journal and census based on operations
#' @param competition List. competitors as returned by the function update_activity.
#' @param simperiod   Character. ID of the period for which the profile holds.
#' @param simworkdays Integer. Number of working days in the period.
#' @param base_market List. market based returned by the function create_case.
#' @importFrom stringr str_remove
#' @importFrom lubridate as_date
#' @importFrom lubridate days_in_month
#' @importFrom dplyr filter 
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider 
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom purrr pmap
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @importFrom dplyr case_when
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom tidyr separate
#' @importFrom dplyr %>%
#' @return Append updated journal and census to the competitors' parameters.
#' @export


update_expenses <- function(competition,
                            simperiod,
                            simworkdays,
                            base_market){
  
  
  # Bind variables
  account <- NULL
  account_label <- NULL
  capacity <- NULL
  costing <- NULL
  credit <- NULL
  debit <- NULL
  destination <- NULL
  destination_label <- NULL
  discount <- NULL
  dpo <- NULL
  duration <- NULL
  from_what <- NULL
  fte <- NULL
  headcount <- NULL
  input <- NULL
  keep <- NULL
  label <- NULL
  nature <- NULL
  object <- NULL
  origin <- NULL
  origin_label <- NULL
  output <- NULL
  parameter <- NULL
  period <- NULL
  price <- NULL
  purpose <- NULL
  quantity <- NULL
  rate <- NULL
  risk <- NULL
  unit <- NULL
  value <- NULL
  vat <- NULL
  working_days <- NULL
  
  
  
  start_date <- simperiod %>%
    stringr::str_remove("P-") %>%
    paste0("-01") %>%
    lubridate::as_date()
  
  end_date <- simperiod %>%
    stringr::str_remove("P-") %>%
    paste0("-", lubridate::days_in_month(start_date)) %>%
    lubridate::as_date()
  
  
  for (i in 1:length(competition)){
    
    
    company <- competition[[i]]
    
    quantities <- company$activity %>%
      dplyr::filter(purpose %in% c("purchases"), period == simperiod) %>%
      dplyr::select(origin = input, destination = output, quantity)
    
    ####################################################################################################
    # Raw materials
    
    materials <- company$capacity %>%
      dplyr::filter(nature == "materials") %>%
      dplyr::select(origin, destination, object = destination_label, parameter, value) %>%
      tidyr::pivot_wider(names_from = parameter, values_from = value) %>%
      dplyr::select(-quantity) %>%
      dplyr::left_join(quantities, by = c("origin", "destination"))
    
    rawmat_entries <- materials %>%
      dplyr::mutate(date = start_date, vat = base_market$environments$value_added_tax[[1]]) %>%
      dplyr::select(date, object, quantity, price, discount, vat, dpo, risk, origin, destination) %>%
      purrr::pmap(simulR::record_purchase) %>%
      dplyr::bind_rows()
    
    materials <- materials %>%
      dplyr::mutate(company = names(competition)[[i]], date = start_date, value = quantity * price) %>%
      dplyr::select(company, date, account = destination, quantity, value)
    
    
    company$census$raw_materials <- company$census$raw_materials %>%
      dplyr::bind_rows(materials)
    
    rm(materials)
    
    ####################################################################################################
    # Services
    
    services_entries <- company$capacity %>%
      dplyr::filter(nature == "services") %>%
      dplyr::select(origin, destination, object = origin_label, parameter, value) %>%
      tidyr::pivot_wider(names_from = parameter, values_from = value) %>%
      dplyr::select(-quantity) %>%
      dplyr::left_join(quantities, by = c("origin", "destination")) %>%
      dplyr::mutate(date = start_date, vat = base_market$environments$value_added_tax[[1]]) %>%
      dplyr::select(date, object, quantity, price, discount, vat, dpo, risk, origin, destination) %>%
      purrr::pmap(simulR::record_purchase) %>%
      dplyr::bind_rows()
    
    
    ####################################################################################################
    # Employment
    
    salaries_entries <- company$capacity %>%
      dplyr::filter(nature == "employment") %>%
      dplyr::select(origin, destination, object = origin_label, parameter, value) %>%
      tidyr::pivot_wider(names_from = parameter, values_from = value) %>%
      dplyr::rename(headcount = quantity) %>%
      dplyr::left_join(quantities, by = c("origin", "destination")) %>%
      dplyr::mutate(working_days = simworkdays) %>%
      dplyr::mutate(capacity = (headcount * capacity * working_days)) %>%
      dplyr::mutate(quantity = dplyr::case_when(
        is.na(quantity) ~ capacity,
        TRUE ~ quantity
      )) %>%
      dplyr::mutate(fte = round(quantity / capacity,1)) %>%
      dplyr::mutate(date = end_date, rate = base_market$environments$labor_tax[[1]]) %>%
      dplyr::select(date, object, quantity = fte, price, rate, origin, destination) %>%
      purrr::pmap(simulR::record_salary) %>%
      dplyr::bind_rows()
    
    
    
    ####################################################################################################
    # Prepaid
    
    prepaid_entries <- company$capacity %>%
      dplyr::select(-unit) %>%
      dplyr::filter(nature == "prepaid") %>%
      tidyr::pivot_wider(names_from = c(parameter), values_from = value) %>%
      dplyr::filter(quantity >= 1) %>%
      dplyr::select(object = origin_label, quantity, price, duration, origin, destination) %>%
      dplyr::mutate(
        date = end_date,
        vat = base_market$environments$value_added_tax,
        risk = 0,
        dpo = 0,
        discount = 0
      ) %>%
      dplyr::select(date, object, quantity, price, discount, vat, dpo, risk, duration, origin, destination) %>%
      purrr::pmap(simulR::record_purchase) %>%
      dplyr::bind_rows()
    
    
    
    ####################################################################################################
    # Production
    
    value_inventory <- company$census$raw_materials %>%
      dplyr::group_by(account) %>%
      dplyr::summarise(quantity = sum(quantity, na.rm = TRUE), value = sum(value, na.rm = TRUE)) %>%
      dplyr::mutate(price = value / quantity) %>%
      dplyr::select(origin = account, price)
    
    consumption <- company$activity %>%
      dplyr::filter(period == simperiod) %>%
      dplyr::left_join(dplyr::select(company$technology, input, output, costing), by = c("input","output")) %>%
      dplyr::filter(costing == "tracing", input > 13300, input < 14000) %>%
      dplyr::select(origin = input, destination = output, quantity, unit) %>%
      dplyr::left_join(value_inventory, by = "origin")
    
    consumption_entries <- consumption %>%
      dplyr::left_join(dplyr::select(base_market$accounts, origin = account, origin_label = account_label), by = "origin") %>%
      dplyr::left_join(dplyr::select(base_market$accounts, destination = account, destination_label = account_label), by = "destination") %>%
      tidyr::separate(origin_label, into = c("from_where","from_what"), sep = " - ") %>%
      dplyr::mutate(unit = dplyr::case_when(quantity > 1 ~ paste0(unit, "s"), TRUE ~ unit)) %>%
      dplyr::mutate(
        date = end_date,
        object = paste0("consumption of ", quantity, " ", unit, " of ", from_what, " for ", destination_label)
      ) %>%
      dplyr::select(date, object, quantity, price, origin, destination) %>%
      purrr::pmap(simulR::record_transfer) %>%
      dplyr::bind_rows()
    
    consumption <- consumption %>%
      dplyr::mutate(value = quantity * price) %>%
      dplyr::group_by(origin) %>%
      dplyr::summarise(quantity = -sum(quantity, na.rm = TRUE), value = -sum(value, na.rm = TRUE)) %>%
      dplyr::mutate(company = names(competition)[[i]], date = end_date) %>%
      dplyr::select(company, date, account = origin, quantity, value)
    
    company$census$raw_materials <- company$census$raw_materials %>%
      dplyr::bind_rows(consumption)
    
    rm(consumption, value_inventory)
    
    ####################################################################################################
    # Append to journal
    
    add_to_journal <- rawmat_entries %>%
      dplyr::bind_rows(services_entries) %>%
      dplyr::bind_rows(salaries_entries) %>%
      dplyr::bind_rows(prepaid_entries) %>%
      dplyr::bind_rows(consumption_entries) %>%
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
      dplyr::mutate(company = names(competition)[[i]]) %>%
      dplyr::select(company, date, label, account, debit, credit)
    
    
    company$journal <- company$journal %>%
      dplyr::bind_rows(add_to_journal)
    
    competition[[i]] <- company
    
    rm(rawmat_entries, services_entries, salaries_entries, prepaid_entries, consumption_entries, add_to_journal, company, quantities)
  }
  
  return(competition)
}



