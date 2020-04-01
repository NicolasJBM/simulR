#' Update information about companies' journal and census based on operations.
#' @param competition       List. competitors as returned by the function pdate_expenses.
#' @param simperiod         Character. ID of the period for which the profile holds.
#' @param base_market       List. market based returned by the function create_case.
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @importFrom lubridate as_date
#' @importFrom lubridate days_in_month
#' @importFrom lubridate month
#' @importFrom dplyr %>%
#' @importFrom dplyr filter 
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom tidyr pivot_wider 
#' @importFrom purrr pmap
#' @importFrom tibble tibble
#' @return Append updated journal and census to the competitors' parameters.
#' @export


update_closing <- function(competition, simperiod, base_market){
  
  
  # Bind variables
  account <- NULL
  account_label <- NULL
  amount <- NULL
  credit <- NULL
  debit <- NULL
  destination <- NULL
  keep <- NULL
  label <- NULL
  object <- NULL
  account_section <- NULL
  
  
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
    
    ###############################################################################
    # Net Income
    income <- company_data$journal %>%
      dplyr::filter(date >= start_date, date <= end_date, account >= 40000, account < 90000) %>%
      dplyr::summarise(debit = sum(debit, na.rm = TRUE), credit = sum(credit, na.rm = TRUE)) %>%
      dplyr::mutate(income = credit - debit) %>%
      dplyr::select(income) %>%
      unlist() %>%
      as.numeric()
    
    
    income_tax <- round(income * base_market$environments$income_tax, 2)
    
    if (income_tax > 0) {
      
      income_tax_entries <- tibble::tibble(
        company = company,
        date = rep(end_date, 2),
        label = "closing - income tax",
        account = c(69000,24400),
        debit = c(income_tax,NA),
        credit = c(NA,income_tax)
      )
      
    } else {
      
      income_tax_entries <- tibble::tibble(
        company = company,
        date = rep(end_date, 2),
        label = "closing - income tax credit",
        account = c(24400, 69000),
        debit = c(abs(income_tax),NA),
        credit = c(NA,abs(income_tax))
      )
      
    }
    
    
    ###############################################################################
    # Net income to retained eanings 
    
    net_income <- income - income_tax
    
    if (net_income < 0){
      
      net_income_entries <- tibble::tibble(
        company = company,
        date = rep(end_date, 2),
        label = "closing - net income (loss)",
        account = c(39000,80000),
        debit = c(abs(net_income),NA),
        credit = c(NA,abs(net_income))
      )
      
    } else {
      
      net_income_entries <- tibble::tibble(
        company = company,
        date = rep(end_date, 2),
        label = "closing - net income (gain)",
        account = c(80000,39000),
        debit = c(abs(net_income),NA),
        credit = c(NA,abs(net_income))
      )
      
    }
    
    journal <- company_data$journal %>%
      dplyr::bind_rows(income_tax_entries) %>%
      dplyr::bind_rows(net_income_entries)
    
    
    ###############################################################################
    # Tax payment
    
    if (lubridate::month(end_date) %in% c(3,6,9,12)){
      
      tax_entries <- journal %>%
        dplyr::filter(account >= 24000, account < 25000) %>%
        dplyr::group_by(account) %>%
        dplyr::summarise(debit = sum(debit, na.rm = TRUE), credit = sum(credit, na.rm = TRUE)) %>%
        dplyr::mutate(date = end_date+10, amount = credit - debit) %>%
        dplyr::left_join(dplyr::select(base_market$accounts, account, object = account_label), by = "account") %>%
        dplyr::select(date, object, amount, origin = account) %>%
        dplyr::filter(amount > 0) %>%
        purrr::pmap(simulR::record_tax_payment) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(company = company) %>%
        dplyr::select(company, dplyr::everything())
      
      journal <- journal %>%
       dplyr::bind_rows(tax_entries)
    }
    
    
    ################################################################################
    
    journal <- journal %>%
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
    
    
    
    ################################################################################
    # Force balance end return
    
    prepgap <- journal %>%
      dplyr::filter(date <= end_date) %>%
      dplyr::left_join(base_market$accounts, by = "account") %>%
      dplyr::filter(account_section %in% c("assets","liabilities or equity")) %>%
      dplyr::group_by(account_section) %>%
      dplyr::summarise(debit = sum(debit, na.rm = TRUE), credit = sum(credit, na.rm = TRUE)) %>%
      dplyr::mutate(gap = dplyr::case_when(
        account_section == "assets" ~ debit - credit,
        TRUE ~ credit - debit
      )) %>%
      dplyr::arrange(account_section)
    
    gap <- prepgap$gap[[1]] - prepgap$gap[[2]]
    
    if (gap >= 0){
      rounding_entries <- tibble::tibble(
        company = company,
        date = end_date,
        label = "closing - correction for rounding",
        account = 39900,
        debit = NA,
        credit = abs(gap)
      )
    } else {
      rounding_entries <- tibble::tibble(
        company = company,
        date = end_date,
        label = "closing - correction for rounding",
        account = 39900,
        debit = abs(gap),
        credit = NA
      )
    }
    
    journal <- journal %>%
      dplyr::bind_rows(rounding_entries) %>%
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
    
    
    company_data$journal <- journal
    competition[[company]] <- company_data
  }
  
  return(competition)
}
