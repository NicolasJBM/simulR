#' Update information about companies' journal and census based on operations.
#' @param competition       List. competitors as returned by the function pdate_expenses.
#' @param simperiod         Character. ID of the period for which the profile holds.
#' @param base_market       List. market based returned by the function create_case.
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @importFrom lubridate as_date
#' @importFrom lubridate days_in_month
#' @importFrom dplyr %>%
#' @importFrom dplyr filter 
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom tidyr pivot_wider 
#' @importFrom tidyr unnest
#' @importFrom purrr pmap
#' @importFrom purrr map2
#' @return Append updated journal and census to the competitors' parameters.
#' @export


update_sales <- function(competition, simperiod, base_market){
  
  
  # Bind variables
  account <- NULL
  account_object <- NULL
  commission <- NULL
  credit <- NULL
  debit <- NULL
  demand <- NULL
  discount <- NULL
  dso <- NULL
  keep <- NULL
  label <- NULL
  nature <- NULL
  object <- NULL
  origin <- NULL
  parameter <- NULL
  period <- NULL
  price <- NULL
  quantity <- NULL
  value <- NULL
  vat <- NULL
  batches <- NULL
  
  
  start_date <- simperiod %>%
    stringr::str_remove("P-") %>%
    paste0("-01") %>%
    lubridate::as_date()
  
  end_date <- simperiod %>%
    stringr::str_remove("P-") %>%
    paste0("-", lubridate::days_in_month(start_date)) %>%
    lubridate::as_date()
  
  
  for (company in names(competition)){
    
  
    company_data <- competition[[company]]
    
    
    unit_cost <- company_data$census$finished_products %>%
      dplyr::group_by(account) %>%
      dplyr::summarise(quantity = sum(quantity, na.rm = TRUE), value = sum(value, na.rm = TRUE)) %>%
      dplyr::mutate(unit_cost = value / quantity) %>%
      dplyr::mutate(account = as.numeric(stringr::str_replace_all(account, "131","400"))) %>%
      dplyr::select(account, unit_cost)
    
    risk <- company_data$capacity %>%
      dplyr::filter(nature == "revenues", parameter == "risk") %>%
      dplyr::select(account = origin, parameter, value) %>%
      tidyr::pivot_wider(names_from = parameter, values_from = value)
    
    sales <- company_data$profile %>%
      dplyr::filter(period == simperiod) %>%
      dplyr::left_join(dplyr::select(base_market$accounts, account, object = account_object), by =  "account") %>%
      dplyr::mutate(batches = purrr::map2(
        object,
        sales,
        simulR::create_batches,
        max_batch_nbr = 20,
        min_batch_size = 20,
        start_date = start_date,
        end_date = end_date
      )) %>%
      dplyr::select(-object) %>%
      tidyr::unnest(batches) %>%
      dplyr::mutate(vat = base_market$environments$value_added_tax) %>%
      dplyr::left_join(unit_cost, by =  "account") %>%
      dplyr::left_join(risk, by =  "account") %>%
      dplyr::select(date,
                    object,
                    quantity,
                    price,
                    discount,
                    vat,
                    dso,
                    unit_cost,
                    sales_commission = commission,
                    risk,
                    origin = account)
    
    
    sales_entries <- sales %>%
      purrr::pmap(simulR::record_sale) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(company = company) %>%
      dplyr::select(company, dplyr::everything())
      
    
    company_data$journal <- company_data$journal %>%
      dplyr::bind_rows(sales_entries) %>%
      dplyr::mutate(
        company = company,
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
      dplyr::select(company, date, label, account, debit, credit)
    
    
    inventory_out <- sales %>%
      dplyr::mutate(origin = as.numeric(stringr::str_replace_all(origin, "400", "131"))) %>%
      dplyr::mutate(value = quantity * unit_cost, date = end_date) %>%
      dplyr::mutate(company = company, quantity = -quantity, value = -value) %>%
      dplyr::select(company, date, account = origin, quantity, value) %>%
      dplyr::group_by(company, date, account) %>%
      dplyr::summarise(quantity = sum(quantity, na.rm = TRUE), value = sum(value, na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    company_data$census$finished_products <- company_data$census$finished_products %>%
      dplyr::bind_rows(inventory_out)
    
    
    competition[[company]] <- company_data
  }
  
  return(competition)
}
