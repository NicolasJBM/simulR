library(tidyverse)

values <- list()

values$simulation <- simulR::calibrate_case()





chart_of_account <- values$simulation$base_market$accounts

journal <- values$simulation$competition[[1]]$journal %>%
  dplyr::filter(!is.na(date), date <= max(values$simulation$base_market$market$date)) %>%
  tidyr::replace_na(list(debit = 0, credit = 0)) %>%
  dplyr::arrange(date, label,-debit,-credit)


activity <- values$simulation$competition[[1]]$activity %>%
  dplyr::group_by(period, input, unit) %>%
  dplyr::summarise(quantity = sum(quantity, na.rp = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(dplyr::select(values$simulation$base_market$accounts, input = account, nature = account_label, purpose = account_object), by = "input") %>%
  dplyr::select(period, nature, purpose, quantity, unit)

profile <- values$simulation$competition[[1]]$profile

usage <- values$simulation$competition[[1]]$activity


WriteXLS::WriteXLS(x = c("chart_of_account","journal","profile","activity","usage"), ExcelFileName = "D:/Downloads/case.xlsx")













incstat <- values$simulation$competition[[1]]$journal %>%
  dplyr::left_join(values$simulation$base_market$accounts, by = "account") %>%
  dplyr::left_join(values$simulation$base_market$market, by = "date") %>%
  dplyr::filter(!is.na(date), !is.na(period)) %>%
  tidyr::replace_na(list(debit = 0, credit = 0)) %>%
  dplyr::filter(account_statement == "income statement") %>%
  dplyr::mutate(amount = credit - debit)


incstat %>%
  dplyr::filter(account_section %in% c("revenues","operating expenses")) %>%
  dplyr::select(period, amount) %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(operating_income = sum(amount, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(product = "all") %>%
  ggplot(aes(x = period, y = operating_income, group = product)) +
  geom_line()


incstat %>%
  dplyr::filter(account_section %in% c("revenues","operating expenses"), account_object != "general") %>%
  dplyr::select(period, product = account_object, amount) %>%
  dplyr::group_by(period, product) %>%
  dplyr::summarise(product_margin = sum(amount, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = period, y = product_margin, group = product, color = product)) +
  geom_line()

incstat %>%
  dplyr::filter(account_category %in% c("sales","sales adjustments","costs of goods sold"), account_object != "general") %>%
  dplyr::select(period, product = account_object, amount) %>%
  dplyr::group_by(period, product) %>%
  dplyr::summarise(gross_margin = sum(amount, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = period, y = gross_margin, group = product, color = product)) +
  geom_line()



























