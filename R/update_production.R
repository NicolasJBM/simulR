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
#' @importFrom dplyr rename
#' @importFrom dplyr case_when
#' @importFrom dplyr summarise
#' @importFrom dplyr summarise_all
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr everything
#' @importFrom tidyr replace_na
#' @importFrom tidyr pivot_wider 
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr pmap
#' @importFrom purrr map_lgl
#' @importFrom purrr map_dbl
#' @importFrom tibble tibble
#' @importFrom tibble rownames_to_column
#' @importFrom tibble column_to_rownames
#' @importFrom tidyselect all_of
#' @return Append updated journal and census to the competitors' parameters.
#' @export




update_production <- function(competition,
                              simperiod,
                              base_market){
  
  
  # Bind variables
  account <- NULL
  account_label <- NULL
  allocation_base <- NULL
  allocation_rate <- NULL
  amount <- NULL
  cost_pool <- NULL
  costing <- NULL
  credit <- NULL
  data <- NULL
  debit <- NULL
  destination <- NULL
  from <- NULL
  from_pool <- NULL
  input <- NULL
  keep <- NULL
  label <- NULL
  method <- NULL
  name <- NULL
  object <- NULL
  object1 <- NULL
  object2 <- NULL
  object_pool <- NULL
  origin <- NULL
  output <- NULL
  period <- NULL
  proportion <- NULL
  proportion_from <- NULL
  purpose <- NULL
  quantity <- NULL
  rank_from <- NULL
  rank_to <- NULL
  received <- NULL
  standard <- NULL
  step <- NULL
  to <- NULL
  to_pool <- NULL
  total_from <- NULL
  type <- NULL
  unit <- NULL
  value <- NULL
  
  distribution <- NULL
  for_product <- NULL
  in_cogs <- NULL
  in_inventory <- NULL
  percentage <- NULL
  sales <- NULL
  price <- NULL
  
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
    
    type_costing <- unique(unlist(company_data$costing$base_costing$type_costing))
    allocation_method <- unique(unlist(company_data$costing$base_costing$allocation_method))
    
    #########################################################################################################
    
    accumulation <- company_data$journal %>%
      dplyr::filter(date >= start_date, date <= end_date, account < 24000 | account >= 40000) %>%
      dplyr::group_by(company, date, label) %>%
      tidyr::nest() %>%
      dplyr::mutate(keep = purrr::map_lgl(data, function(x) sum(x$account[[1]] >= 90000) == 1)) %>%
      dplyr::filter(keep == TRUE) %>%
      dplyr::mutate(data = purrr::map(
        data,
        function(x) tibble::tibble(from = x$account[[2]], to = x$account[[1]], value = x$debit[[1]])
      )) %>%
      tidyr::unnest(data) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(from, to) %>%
      dplyr::summarise(accumulated = sum(value, na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    
    #########################################################################################################
    
    services_distribution <- company_data$technology %>%
      dplyr::filter(purpose %in% c("production","support"), costing == "allocation") %>%
      dplyr::select(input, output, from_pool, to_pool) %>%
      dplyr::left_join(dplyr::filter(company_data$activity, period == simperiod), by = c("input","output")) %>%
      dplyr::select(from_pool, to_pool, quantity) %>%
      dplyr::mutate(from = dplyr::case_when(
        substr(from_pool, 1,3) == "910" ~ "cost object",
        TRUE ~ "cost pool"
      )) %>%
      dplyr::mutate(to = dplyr::case_when(
        substr(to_pool, 1,3) == "910" ~ "cost object",
        TRUE ~ "cost pool"
      )) %>%
      dplyr::mutate(type = dplyr::case_when(
        from_pool == to_pool ~ "self service",
        from == to ~ "reciprocal service",
        TRUE ~ "service"
      )) %>%
      dplyr::group_by(from_pool, from) %>%
      tidyr::nest() %>%
      dplyr::mutate(total_from = purrr::map_dbl(data, function(x) sum(x$quantity))) %>%
      tidyr::unnest(data) %>%
      dplyr::mutate(proportion_from = quantity / total_from) %>%
      dplyr::ungroup()
    
    rank_pool <- services_distribution %>%
      dplyr::filter(type != "self service") %>%
      dplyr::group_by(from_pool, to) %>%
      dplyr::summarise(proportion = sum(proportion_from)) %>%
      dplyr::filter(to == "cost object") %>%
      dplyr::arrange(proportion) %>%
      tibble::rownames_to_column("rank") %>%
      dplyr::mutate(rank = as.numeric(rank)) %>%
      dplyr::ungroup() %>%
      dplyr::select(account = from_pool, rank)
    
    rank_object <- services_distribution %>%
      dplyr::filter(to == "cost object") %>%
      dplyr::select(to_pool) %>%
      unique() %>%
      dplyr::arrange(to_pool) %>%
      tibble::rownames_to_column("rank") %>%
      dplyr::mutate(rank = as.numeric(rank)) %>%
      dplyr::mutate(rank = rank + max(rank_pool$rank)) %>%
      dplyr::select(account = to_pool, rank)
    
    ranks <- rank_pool %>%
      dplyr::bind_rows(rank_object) %>%
      dplyr::arrange(rank)
    
    rm(rank_pool, rank_object)
    
    services_distribution <- services_distribution %>%
      dplyr::left_join(dplyr::select(ranks, from_pool = account, rank_from = rank), by = "from_pool") %>%
      dplyr::left_join(dplyr::select(ranks, to_pool = account, rank_to = rank), by = "to_pool")
    
    rm(ranks)
    
    
    #########################################################################################################
    
    accumulated <- services_distribution %>%
      dplyr::arrange(rank_from) %>%
      dplyr::select(from_pool) %>%
      unique() %>%
      dplyr::left_join(dplyr::select(accumulation, from_pool = to, accumulated), by = "from_pool") %>%
      dplyr::group_by(from_pool) %>%
      dplyr::summarise(accumulated = sum(accumulated, na.rm = TRUE))
    
    
    if (allocation_method == "reciprocal"){
      
      
      allocation_base_distribution <- services_distribution %>%
        dplyr::mutate(quantity = dplyr::case_when(
          type == "self service" ~ 0,
          TRUE ~ quantity
        )) %>%
        dplyr::group_by(from_pool, from) %>%
        dplyr::select(-total_from) %>%
        tidyr::nest() %>%
        dplyr::mutate(total_from = purrr::map_dbl(data, function(x) sum(x$quantity))) %>%
        tidyr::unnest(data) %>%
        dplyr::mutate(proportion_from = quantity / total_from) %>%
        dplyr::ungroup()
      
      matrix <- allocation_base_distribution %>%
        dplyr::mutate(proportion_from = -proportion_from) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(rank_to) %>%
        dplyr::select(from_pool, rank_from, to_pool, proportion_from) %>%
        tidyr::pivot_wider(names_from = c("to_pool"), values_from = c("proportion_from"), values_fill = list(proportion_from = 0)) %>%
        tidyr::pivot_longer(cols = c(as.character(unique(services_distribution$to_pool))), names_to = c("to_pool"), values_to = c("proportion_from")) %>%
        dplyr::mutate(proportion_from = dplyr::case_when(
          from_pool == to_pool ~ 1,
          TRUE ~ proportion_from
        )) %>%
        tidyr::pivot_wider(names_from = c("to_pool"), values_from = c("proportion_from"), values_fill = list(proportion_from = 0)) %>%
        dplyr::arrange(rank_from) %>%
        dplyr::select(-rank_from) %>%
        as.data.frame() %>%
        dplyr::mutate(from_pool = as.character(from_pool)) %>%
        as.data.frame() %>%
        tibble::column_to_rownames("from_pool") %>%
        dplyr::select(as.character(accumulated$from_pool)) %>%
        as.matrix() %>%
        t()
      
      vector <- accumulated %>%
        as.data.frame() %>%
        tibble::column_to_rownames("from_pool") %>%
        as.matrix()
      
      allocation_rates <- solve(matrix,vector) %>%
        as.data.frame() %>%
        tibble::rownames_to_column("from_pool") %>%
        dplyr::rename(allocation = accumulated) %>%
        dplyr::mutate(from_pool = as.numeric(from_pool)) %>%
        dplyr::left_join(accumulated, by = "from_pool") %>%
        dplyr::left_join(unique(select(allocation_base_distribution, from_pool, allocation_base = total_from)), by = "from_pool") %>%
        dplyr::mutate(
          costing = type_costing,
          method = allocation_method,
          allocated = allocation - accumulated,
          allocation_rate = allocation / allocation_base
        ) %>%
        dplyr::select(costing, method, cost_pool = from_pool, accumulated, allocated, allocation, allocation_base, allocation_rate)
      
      
      rm(accumulated, matrix, vector)
      
    } else if (allocation_method == "sequential"){
      
      allocation_base_distribution <- services_distribution %>%
        dplyr::mutate(quantity = dplyr::case_when(
          type == "self service" ~ 0,
          rank_from > rank_to ~ 0,
          TRUE ~ quantity
        )) %>%
        dplyr::group_by(from_pool, from) %>%
        dplyr::select(-total_from) %>%
        tidyr::nest() %>%
        dplyr::mutate(total_from = purrr::map_dbl(data, function(x) sum(x$quantity))) %>%
        tidyr::unnest(data) %>%
        dplyr::mutate(proportion_from = quantity / total_from) %>%
        dplyr::ungroup()
      
      matrix <- allocation_base_distribution %>%
        dplyr::ungroup() %>%
        dplyr::arrange(rank_to) %>%
        dplyr::select(from_pool, rank_from, to_pool, quantity) %>%
        tidyr::pivot_wider(names_from = c("to_pool"), values_from = c("quantity"), values_fill = list(quantity = 0)) %>%
        dplyr::arrange(rank_from) %>%
        as.data.frame()
      
      allocation_rates <- matrix
      
      allocation_rates$allocation_base = rowSums(matrix[,c(3:length(allocation_rates))])
      
      allocation_rates <- allocation_rates %>%
        dplyr::left_join(accumulated, by = "from_pool") %>%
        dplyr::mutate(allocated = 0, allocation_rate = NA) %>%
        dplyr::select(rank = rank_from, from_pool, accumulated, allocated, allocation_base)
      
      allocated <- matrix %>%
        tidyr::pivot_longer(cols = as.character(unique(accumulation$to)), names_to = c("to_pool"), values_to = c("quantity")) %>%
        dplyr::select(from_pool, to_pool, quantity) %>%
        dplyr::filter(to_pool %in% allocation_rates$from_pool, quantity > 0)
      
      
      for (i in 1:nrow(allocation_rates)){
        
        allrate <- (allocation_rates[i,"accumulated"] + allocation_rates[i,"allocated"]) / allocation_rates[i,"allocation_base"]
        
        allocation_rates[i,"allocation_rate"] <- allrate
        tmp <- dplyr::filter(allocated, from_pool == allocation_rates[i,"from_pool"]) %>%
          dplyr::mutate(from_pool = as.numeric(to_pool), received = quantity * allrate) %>%
          dplyr::select(from_pool, received)
        
        allocation_rates <- allocation_rates %>%
          dplyr::left_join(tmp, by = "from_pool") %>%
          tidyr::replace_na(list(received = 0)) %>%
          dplyr::mutate(allocated = allocated + received) %>%
          dplyr::select(-received)
        
        rm(allrate, tmp)
      }
      
      allocation_rates <- allocation_rates %>%
        dplyr::mutate(costing = type_costing, method = allocation_method) %>%
        dplyr::arrange(rank) %>%
        dplyr::mutate(allocation = accumulated + allocated) %>%
        dplyr::select(costing, method, cost_pool = from_pool, accumulated, allocated, allocation, allocation_base, allocation_rate)
      
      rm(accumulated, allocated, matrix, i)
      
    } else {
      
      allocation_base_distribution <- services_distribution %>%
        dplyr::mutate(quantity = dplyr::case_when(
          type != "service" ~ 0,
          TRUE ~ quantity
        )) %>%
        dplyr::group_by(from_pool, from) %>%
        dplyr::select(-total_from) %>%
        tidyr::nest() %>%
        dplyr::mutate(total_from = purrr::map_dbl(data, function(x) sum(x$quantity))) %>%
        tidyr::unnest(data) %>%
        dplyr::mutate(proportion_from = quantity / total_from) %>%
        dplyr::ungroup()
      
      matrix <- allocation_base_distribution %>%
        dplyr::ungroup() %>%
        dplyr::arrange(rank_to) %>%
        dplyr::select(from_pool, rank_from, to_pool, quantity) %>%
        tidyr::pivot_wider(names_from = c("to_pool"), values_from = c("quantity"), values_fill = list(quantity = 0)) %>%
        dplyr::arrange(rank_from) %>%
        as.data.frame()
      
      allocation_rates <- matrix
      
      allocation_rates$allocation_base = rowSums(matrix[,c(3:length(allocation_rates))])
      
      allocation_rates <- allocation_rates %>%
        dplyr::left_join(accumulated, by = "from_pool") %>%
        dplyr::mutate(allocated = 0) %>%
        dplyr::select(rank = rank_from, cost_pool = from_pool, accumulated, allocated, allocation_base) %>%
        dplyr::mutate(costing = type_costing, method = allocation_method, allocation = accumulated, allocation_rate = accumulated / allocation_base) %>%
        dplyr::select(costing, method, cost_pool, accumulated, allocated, allocation, allocation_base, allocation_rate)
      
      rm(accumulated, matrix)
    }
    
    
    #########################################################################################################
    
    order <- services_distribution %>%
      dplyr::select(to_pool, rank_to) %>% 
      unique() %>%
      dplyr::arrange(rank_to) %>%
      dplyr::select(to_pool) %>%
      unlist() %>%
      as.character()
    
    allocation <- allocation_base_distribution %>%
      dplyr::select(rank_from, from_pool, to_pool, quantity) %>%
      tidyr::pivot_wider(names_from = to_pool, values_from = quantity, values_fill = list(quantity = 0)) %>%
      dplyr::select(rank_from, from_pool, tidyselect::all_of(order)) %>%
      dplyr::arrange(rank_from) %>%
      dplyr::select(-rank_from) %>%
      as.data.frame() %>%
      tibble::column_to_rownames("from_pool")
    
    if (type_costing == "actual"){
      
      apply_rates <- allocation_rates %>%
        dplyr::select(cost_pool, allocation_rate) %>%
        as.data.frame() %>%
        tibble::column_to_rownames("cost_pool")
      
    } else {
      
      apply_rates <- company_data$costing$base_costing %>%
        dplyr::select(object_pool, allocation_rate = paste0("standard_cost_", allocation_method)) %>%
        as.data.frame() %>%
        tibble::column_to_rownames("object_pool")
      
    }
    
    
    #########################################################################################################
    
    prep_assignment_table1 <- allocation
    rates <- apply_rates[rownames(prep_assignment_table1),1]
    for (i in 1:length(prep_assignment_table1)) prep_assignment_table1[,i] <- prep_assignment_table1[,i] * rates
    
    
    for (i in 1:nrow(prep_assignment_table1)) prep_assignment_table1[i,i] <- -sum(prep_assignment_table1[i,])
    
    prep_assignment_table1 <- prep_assignment_table1 %>%
      as.data.frame() %>%
      tibble::rownames_to_column("from") %>%
      dplyr::mutate(step = "allocation", from = as.numeric(from)) %>%
      dplyr::select(step, from, tidyselect::all_of(order))
    
    prep_assignment_table2 <- accumulation %>%
      tidyr::pivot_wider(names_from = to, values_from = accumulated, values_fill = list(accumulated = 0)) %>%
      dplyr::mutate(step = "accumulation") %>%
      dplyr::select(step, from, tidyselect::all_of(order))
    
    assignment_table <- prep_assignment_table2 %>%
      dplyr::bind_rows(prep_assignment_table1)
    
    
    #########################################################################################################
    
    apply_rates <- apply_rates %>%
      tibble::rownames_to_column("origin")
    
    allocation <- allocation %>%
      as.data.frame() %>%
      tibble::rownames_to_column("origin") %>%
      tidyr::pivot_longer(cols = order, names_to = "destination", values_to = "allocation_base") %>%
      dplyr::left_join(apply_rates, by = "origin")
    
    
    if (type_costing == "actual") type_rate <- "an actual" else type_rate <- "a standard"
    
    allocation_entries <- allocation %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::left_join(dplyr::select(base_market$accounts, origin = account, object1 = account_label), by = "origin") %>%
      dplyr::left_join(dplyr::select(base_market$accounts, destination = account, object2 = account_label), by = "destination") %>%
      dplyr::left_join(dplyr::select(company_data$costing$base_costing, origin = object_pool, name = allocation_base), by = "origin") %>%
      dplyr::mutate(
        date = end_date,
        object = paste0("allocation of ", round(allocation_base,2), " ", name, " at ", type_rate, " rate of ", round(allocation_rate,2), " from ", object1, " to ", object2)
      ) %>%
      dplyr::select(date, object, allocation_base, allocation_rate, origin, destination) %>%
      dplyr::filter(allocation_base > 0) %>%
      purrr::pmap(simulR::record_allocation) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(company = company) %>%
      dplyr::select(company, dplyr::everything())
    
    rm(type_rate)
    
    allocation <- allocation %>%
      dplyr::filter(destination >= 91000, destination < 92000) %>%
      dplyr::mutate(percentage = allocation_base * allocation_rate) %>%
      dplyr::select(origin, destination, percentage) %>%
      dplyr::group_by(origin) %>%
      dplyr::mutate(percentage = percentage / sum(percentage)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_all(as.numeric)
    
    journal <- company_data$journal %>%
      dplyr::bind_rows(allocation_entries)
    
    rm(prep_assignment_table1, prep_assignment_table2, rates, apply_rates, allocation_base_distribution, accumulation, allocation_entries)
    
    
    #########################################################################################################
    
    production <- company_data$activity %>%
      dplyr::filter(period == simperiod, output >= 13100, output < 13200, unit == "unit") %>%
      dplyr::select(destination = output, quantity) %>%
      dplyr::mutate(origin = as.numeric(stringr::str_replace_all(destination, "131", "910"))) %>%
      dplyr::left_join(select(company_data$costing$base_costing, origin = object_pool, standard = paste0("standard_cost_", allocation_method)), by = "origin") %>%
      dplyr::select(origin, destination, quantity, standard)
    
    
    if (type_costing == "standard"){
      
      standard_transfer <- production %>%
        dplyr::mutate(date = end_date) %>%
        dplyr::left_join(dplyr::select(base_market$accounts, origin = account, object1 = account_label), by = "origin") %>%
        dplyr::left_join(dplyr::select(base_market$accounts, destination = account, object2 = account_label), by = "destination") %>%
        dplyr::mutate(
          date = end_date,
          object = paste0("transfer of ", quantity, " units at a standard cost of ", standard, " from ", object1, " to ", object2)
        ) %>%
        dplyr::select(date, object, quantity, price = standard, origin, destination) %>%
        purrr::pmap(simulR::record_transfer) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(company = company) %>%
        dplyr::select(company, dplyr::everything())
      
      add_inventory <- production %>%
        dplyr::mutate(company = company, date = end_date, value = quantity * standard) %>%
        dplyr::select(company, date, account = destination, quantity, value)
      
      company_data$census$finished_products <- company_data$census$finished_products %>%
        dplyr::bind_rows(add_inventory)
      
      journal <- dplyr::bind_rows(journal, standard_transfer)
      rm(standard_transfer)
      
    } else {
      
      prep_add <- journal %>%
        dplyr::filter(account >= 91000, account < 92000, date >= start_date, date <= end_date) %>%
        dplyr::select(origin = account, debit, credit) %>%
        tidyr::replace_na(list(debit = 0, credit = 0)) %>%
        dplyr::mutate(value = debit - credit) %>%
        dplyr::group_by(origin) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))
      
      add_inventory <- production %>%
        dplyr::left_join(prep_add, by = "origin") %>%
        dplyr::mutate(company = company, date = end_date) %>%
        dplyr::select(company, date, account = destination, quantity, value)
      
      transfer <- add_inventory %>%
        dplyr::mutate(
          origin = as.numeric(stringr::str_replace_all(account, "131", "910")),
          destination = account,
          price = value / quantity
        ) %>%
        dplyr::select(-account, -company) %>%
        dplyr::left_join(dplyr::select(base_market$accounts, origin = account, object1 = account_label), by = "origin") %>%
        dplyr::left_join(dplyr::select(base_market$accounts, destination = account, object2 = account_label), by = "destination") %>%
        dplyr::mutate(
          object = paste0("transfer of ", quantity, " units at an actual cost of ", round(price,2), " from ", object1, " to ", object2)
        ) %>%
        dplyr::select(date, object, quantity, price, origin, destination) %>%
        purrr::pmap(simulR::record_transfer) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(company = company) %>%
        dplyr::select(company, dplyr::everything())
      
      company_data$census$finished_products <- company_data$census$finished_products %>%
        dplyr::bind_rows(add_inventory)
      
      journal <- dplyr::bind_rows(journal, transfer)
      
      rm(prep_add, transfer)
    }
    
    
    #########################################################################################################
    
    
    prep_pool_reset <- journal %>%
      dplyr::filter(account %in% order, date <= end_date) %>%
      dplyr::select(origin = account, debit, credit) %>%
      dplyr::group_by(origin) %>%
      dplyr::summarise_all(sum, na.rm = TRUE) %>%
      dplyr::mutate(amount = debit - credit) %>%
      dplyr::mutate(type = dplyr::case_when(
        origin >= 91000 & origin < 92000 ~ "object",
        TRUE ~ "pool"
      ))
    
    prep_pool_reset <- split(prep_pool_reset, prep_pool_reset$type)
    
    
    
    adjustment_distribution <- company_data$profile %>%
      dplyr::filter(period == simperiod) %>%
      dplyr::select(account, sales) %>%
      dplyr::mutate(account = as.numeric(stringr::str_replace_all(account, "400","131"))) %>%
      dplyr::left_join(add_inventory, by = "account") %>%
      dplyr::mutate(
        in_inventory = dplyr::case_when(
          quantity <= sales ~ 0,
          TRUE ~ (quantity - sales) / quantity
        )
      ) %>%
      dplyr::mutate(in_cogs = 1 - in_inventory)
    
    
    prep_pool_reset$object <- prep_pool_reset$object %>%
      dplyr::mutate(account = as.numeric(stringr::str_replace_all(origin, "910","131"))) %>%
      dplyr::left_join(adjustment_distribution, by = "account") %>%
      dplyr::mutate(
        in_inventory = in_inventory * amount,
        in_cogs = in_cogs * amount
      ) %>%
      dplyr::select(origin, destination = account, in_inventory, in_cogs) %>%
      tidyr::pivot_longer(cols = c("in_inventory","in_cogs"), names_to = "where", values_to = "amount") %>%
      dplyr::mutate(destination = dplyr::case_when(
        where == "in_inventory" ~ destination,
        TRUE ~ as.numeric(stringr::str_replace_all(destination, "131", "582"))
      )) %>%
      dplyr::select(amount, origin, destination) %>%
      dplyr::filter(amount != 0)
    
    
    adjustment_distribution <- adjustment_distribution %>%
      dplyr::select(destination = account, in_inventory, in_cogs) %>%
      dplyr::mutate(destination = as.numeric(stringr::str_replace_all(destination, "131", "910"))) %>%
      tidyr::pivot_longer(cols = c("in_inventory","in_cogs"), names_to = "where", values_to = "proportion")
    
    prep_pool_reset$pool <- prep_pool_reset$pool %>%
      dplyr::left_join(dplyr::select(allocation, origin, destination, percentage), by = "origin") %>%
      dplyr::left_join(adjustment_distribution, by = "destination") %>%
      dplyr::mutate(amount = amount * percentage * proportion) %>%
      dplyr::mutate(destination = dplyr::case_when(
        where == "in_inventory" ~ as.numeric(stringr::str_replace_all(destination, "910", "131")),
        TRUE ~ as.numeric(stringr::str_replace_all(destination, "910", "582"))
      )) %>%
      dplyr::select(amount, origin, destination) %>%
      dplyr::filter(amount != 0)
     
    prep_pool_reset <- dplyr::bind_rows(prep_pool_reset) %>%
      dplyr::mutate(date = end_date) %>%
      dplyr::left_join(dplyr::select(base_market$accounts, destination = account, object = account_label), by = "destination") %>%
      dplyr::mutate(object = paste0("clear transitory account to ", object)) %>%
      dplyr::select(date, object, amount, origin, destination)
    
    
    pool_reset_entries <- prep_pool_reset %>%
      purrr::pmap(simulR::record_pool_reset) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(company = company) %>%
      dplyr::select(company, dplyr::everything())
    
    rm(add_inventory, prep_pool_reset, adjustment_distribution)
    
    #########################################################################################################
    
    journal <- dplyr::bind_rows(journal, pool_reset_entries) %>%
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
    
    rm(pool_reset_entries)
    
    
    
    #########################################################################################################
    
    company_data$journal <- journal
    
    services_distribution <- services_distribution %>%
      dplyr::mutate(company = company, period = simperiod) %>%
      dplyr::select(company, period, dplyr::everything())
    
    company_data$costing$services_distribution <- company_data$costing$services_distribution %>%
      dplyr::bind_rows(services_distribution) %>%
      na.omit()
    
    allocation_rates <- allocation_rates %>%
      dplyr::mutate(company = company, period = simperiod) %>%
      dplyr::select(company, period, dplyr::everything())
    
    company_data$costing$allocation_rates <- company_data$costing$allocation_rates %>%
      dplyr::bind_rows(allocation_rates) %>%
      na.omit()
    
    assignment_table <- assignment_table %>%
      dplyr::mutate(company = company, period = simperiod) %>%
      dplyr::select(company, period, dplyr::everything())
    
    company_data$costing$assignment_table <- company_data$costing$assignment_table %>%
      dplyr::bind_rows(assignment_table) %>%
      na.omit()
    
    competition[[company]] <- company_data
  }
  
  return(competition)
}

