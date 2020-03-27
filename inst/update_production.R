type_costing = "normal"
allocation_method = "direct"

simperiod = periodic_demand$period[[period_nbr]]


start_date <- simperiod %>%
  stringr::str_remove("P-") %>%
  paste0("-01") %>%
  lubridate::as_date()

end_date <- simperiod %>%
  stringr::str_remove("P-") %>%
  paste0("-", lubridate::days_in_month(start_date)) %>%
  lubridate::as_date()


company <- competition[[1]]

accumulation <- company$journal %>%
  dplyr::filter(date >= start_date, date <= end_date) %>%
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
  



services_distribution <- company$technology %>%
  dplyr::filter(purpose %in% c("production","support"), costing == "allocation") %>%
  dplyr::select(input, output, from_pool, to_pool) %>%
  dplyr::left_join(company$activity, by = c("input","output")) %>%
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
  #pivot_wider(names_from = to_pool, values_from = quantity)

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
    dplyr::mutate(allocated = 0) %>%
    dplyr::select(rank = rank_from, cost_pool = from_pool, accumulated, allocated, allocation_base)
  
  allocated <- matrix %>%
    tidyr::pivot_longer(cols = as.character(unique(accumulation$to)), names_to = c("to_pool"), values_to = c("quantity")) %>%
    dplyr::select(cost_pool = from_pool, to_pool, quantity) %>%
    dplyr::filter(to_pool %in% allocation_rates$cost_pool)
  
  allocation_rates <- split(allocation_rates, allocation_rates$cost_pool)
  allocated <- split(allocated, allocated$cost_pool)
  
  for (i in 1:length(allocation_rates)){
    
    allocation_rates[[i]] <- allocation_rates[[i]] %>%
      dplyr::mutate(allocation = accumulated + allocated) %>%
      dplyr::mutate(allocation_rate = allocation / allocation_base)
    
    allocated[[i]] <- allocated[[i]] %>%
      dplyr::mutate(allocation_rate = allocation_rates[[i]]$allocation_rate[[1]]) %>%
      dplyr::mutate(allocated = quantity * allocation_rate) %>%
      dplyr::select(to_pool, allocated)
    
    for (j in 1:length(allocation_rates)) allocation_rates[[j]]$allocated <- allocation_rates[[j]]$allocated + allocated[[i]]$allocated[[j]]
    
  }
  
  allocation_rates <- allocation_rates %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(costing = type_costing, method = allocation_method) %>%
    dplyr::arrange(rank) %>%
    dplyr::select(costing, method, cost_pool, accumulated, allocated, allocation, allocation_base, allocation_rate)
  
  
  rm(accumulated, allocated, matrix, i, j)
  
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
  dplyr::select(rank_from, from_pool, order) %>%
  dplyr::arrange(rank_from) %>%
  dplyr::select(-rank_from) %>%
  as.data.frame() %>%
  tibble::column_to_rownames("from_pool")


 
if (type_costing == "actual"){
  
  apply_rates <- allocation_rates %>%
    dplyr::select(cost_pool, allocation_rate) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("cost_pool")
  apply_rates <- apply_rates[rownames(allocation),1]
  
} else if (type_costing == "normal") {
  
  apply_rates <- company$costing$base_costing %>%
    dplyr::select(object_pool, allocation_rate = paste0("standard_cost_", allocation_method)) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("object_pool")
  apply_rates <- apply_rates[rownames(allocation),1]
  
}


for (i in 1:length(allocation)) allocation[,i] <- allocation[,i] * apply_rates


prep_assignment1 <- allocation

for (i in 1:nrow(prep_assignment1)) prep_assignment1[i,i] <- -sum(prep_assignment1[i,])

prep_assignment1 <- prep_assignment1 %>%
  as.data.frame() %>%
  tibble::rownames_to_column("assignment") %>%
  dplyr::mutate(step = "allocation", from = as.numeric(assignment)) %>%
  dplyr::select(step, from, order)

prep_assignment2 <- accumulation %>%
  tidyr::pivot_wider(names_from = to, values_from = accumulated, values_fill = list(accumulated = 0)) %>%
  dplyr::mutate(step = "accumulation") %>%
  dplyr::select(step, from, order)

assignment <- prep_assignment2 %>%
  dplyr::bind_rows(prep_assignment1)

allocation <- allocation %>%
  as.data.frame() %>%
  tibble::rownames_to_column("from") %>%
  tidyr::pivot_longer(cols = order, names_to = "to", values_to = "allocated")

rm(prep_assignment1, prep_assignment2, apply_rates, order, allocation_base_distribution)






round(colSums(assignment[,c(3:8)]),2)
dplyr::filter(company$activity, output == 13110)
dplyr::filter(company$activity, output == 13120)
dplyr::filter(company$activity, output == 13130)
                                                      