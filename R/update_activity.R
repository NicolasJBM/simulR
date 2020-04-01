#' Update information about companies' competitive profiles in competition. 
#' @param competition List. competitors as returned by the function update_profile.
#' @param simperiod   Character. ID of the period for which the profile holds.
#' @param simworkdays Integer. Number of working days in the period.
#' @importFrom dplyr %>%
#' @importFrom dplyr filter 
#' @importFrom dplyr summarise
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr everything
#' @importFrom dplyr arrange
#' @importFrom dplyr full_join
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_wider 
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @importFrom stats na.omit
#' @importFrom purrr map_dbl
#' @importFrom purrr map2_dbl
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @return Append updated activity to the competitors' parameters.
#' @export


update_activity <- function(competition, simperiod, simworkdays){
  
  
  # bind variables
  account <- NULL
  available <- NULL
  batch_consumption <- NULL
  batches_in_production <- NULL
  capacity <- NULL
  consumed <- NULL
  nature <- NULL
  purpose <- NULL
  demand <- NULL
  destination <- NULL
  employment <- NULL
  variability <- NULL
  beginning <- NULL
  input_standard_quantity <- NULL
  input_unit <- NULL
  maximum <- NULL
  minimum <- NULL
  needed <- NULL
  input <- NULL
  input_label <- NULL
  origin <- NULL
  output <- NULL
  output_standard_quantity <- NULL
  parameter <- NULL
  period <- NULL
  purpose <- NULL
  plan <- NULL
  priority <- NULL
  production_needed <- NULL
  quantity <- NULL
  sales <- NULL
  unit <- NULL
  units_in_production <- NULL
  value <- NULL
  produced <- NULL
  purchased <- NULL
  sold <- NULL
  unused <- NULL
  tmp_sales <- NULL
  units_available <- NULL
  
  
  for (company in names(competition)){
    
    ##########################################################################################################
    # Gather information about inventories, purchases and production
    
    beginning <- list()
    
    beginning$finpro <- competition[[company]]$census$finished_products %>%
      dplyr::group_by(account) %>%
      dplyr::summarise(beginning = sum(quantity))
    beginning$rawmat <- competition[[company]]$census$raw_materials %>%
      dplyr::group_by(account) %>%
      dplyr::summarise(beginning = sum(quantity))
    beginning$assets <- competition[[company]]$census$assets %>%
      dplyr::mutate(beginning = capacity * simworkdays) %>%
      dplyr::group_by(account) %>%
      dplyr::summarise(beginning = sum(beginning))
    beginning$people <- competition[[company]]$census$people %>%
      dplyr::mutate(beginning = capacity * simworkdays) %>%
      dplyr::group_by(account) %>%
      dplyr::summarise(beginning = sum(beginning))
    
    beginning <- dplyr::bind_rows(beginning) %>%
      dplyr::ungroup()
    
    
    obtainable <- list()
    
    obtainable$purchases <- competition[[company]]$capacity %>%
      dplyr::filter(nature %in% c("materials","services")) %>%
      tidyr::pivot_wider(names_from = c("parameter"), values_from = "value") %>%
      dplyr::select(origin, destination, nature, purpose, quantity, variability) %>%
      dplyr::mutate(account = dplyr::case_when(
        nature == "materials" ~ destination,
        nature == "services" & purpose == "selling" ~ destination,
        TRUE ~ origin
      )) %>%
      dplyr::group_by(account) %>%
      dplyr::summarise(available = sum(quantity, na.rm = TRUE), variability = mean(variability, na.rm = TRUE)) %>%
      dplyr::mutate(
        minimum = available * (1-variability),
        maximum = available * (1+variability)
      ) %>%
      dplyr::select(account, minimum, maximum) %>%
      na.omit()
    
    
    obtainable$labor <- competition[[company]]$capacity %>%
      dplyr::filter(nature == "employment") %>%
      dplyr::select(account = origin, parameter, value) %>%
      tidyr::pivot_wider(names_from = c("parameter"), values_from = "value") %>%
      dplyr::mutate(available = quantity * capacity * simworkdays) %>%
      dplyr::mutate(
        minimum = available * (1-variability),
        maximum = available * (1+variability),
      ) %>%
      dplyr::select(account, minimum, maximum) %>%
      na.omit()
    
    
    obtainable$production <- competition[[company]]$capacity %>%
      dplyr::filter(nature == "cogs") %>%
        dplyr::select(account = destination, parameter, value) %>%
      tidyr::pivot_wider(names_from = c("parameter"), values_from = "value") %>%
      dplyr::mutate(available = quantity) %>%
      dplyr::mutate(
        minimum = 0,
        maximum = available,
      ) %>%
      dplyr::select(account, minimum, maximum) %>%
      na.omit()
    
    obtainable <- dplyr::bind_rows(obtainable)
    
    resources_available <- beginning %>%
      dplyr::full_join(obtainable, by = "account") %>%
      tidyr::replace_na(list(beginning = 0, minimum = 0, maximum = 0)) %>%
      dplyr::mutate(minimum = minimum + beginning, maximum = maximum + beginning) %>%
      dplyr::group_by(account) %>%
      dplyr::summarise(beginning = sum(beginning), minimum = sum(minimum), maximum = sum(maximum)) %>%
      dplyr::mutate(available = maximum)
    
    rm(beginning, obtainable)
    
    
    ##########################################################################################################
    
    
    technology <- competition[[company]]$technology %>%
      dplyr::select(-company)
    
    demand_plan <- competition[[company]]$profile %>%
      dplyr::filter(period == simperiod) %>%
      dplyr::select(account, demand) %>%
      dplyr::mutate(account = as.numeric(stringr::str_replace_all(account, "400", "131"))) %>%
      dplyr::mutate(plan = purrr::map_dbl(account, function(x,y) dplyr::filter(y, account == x)$available[[1]], y = resources_available))
    
    schedules <- competition[[company]]$capacity %>%
      dplyr::filter(purpose == "production", parameter == "priority") %>%
      dplyr::select(account = destination, priority = value) %>%
      dplyr::left_join(demand_plan, by = "account") %>%
      dplyr::left_join(dplyr::select(resources_available, account, beginning), by = "account") %>%
      dplyr::mutate(needed = purrr::map2_dbl(demand, plan, max)) %>%
      dplyr::mutate(production_needed = needed - beginning) %>%
      dplyr::select(account, priority, demand, beginning, production_needed) %>%
      dplyr::arrange(priority) %>%
      dplyr::mutate(account = as.numeric(stringr::str_replace_all(account, "131", "910")))
    
    rm(demand_plan)
    
    # simulate the planned production and the related consumption of resources, in order of priority, until one resource is missing
    
    batches <- c()
    units <- c()
    consumptions <- list()
    
    for (i in 1:nrow(schedules)){
      
      product <- schedules$account[[i]]
      
      # First, compte resources directly and indirectly consumed by every batch
      direct_batch_consumption <- technology %>%
        dplyr::filter(purpose == "production", output == product) %>%
        dplyr::select(input, input_label, batch_consumption = input_standard_quantity)
      
      indirect_batch_consumption <- technology %>%
        dplyr::filter(purpose == "support", output %in% direct_batch_consumption$input) %>%
        dplyr::left_join(dplyr::select(direct_batch_consumption, output = input, batch_consumption), by = "output") %>%
        dplyr::mutate(batch_consumption = batch_consumption * input_standard_quantity / output_standard_quantity) %>%
        dplyr::select(input, input_label, batch_consumption)
      
      consumption_per_batch <- direct_batch_consumption %>%
        dplyr::bind_rows(indirect_batch_consumption) %>%
        dplyr::select(account = input, batch_consumption) %>%
        dplyr::group_by(account) %>%
        dplyr::summarise(batch_consumption = sum(batch_consumption)) %>%
        dplyr::left_join(dplyr::select(resources_available, account, available), by = "account") %>%
        dplyr::mutate(possible_batches = floor(available / batch_consumption)) %>%
        na.omit()
      
      rm(direct_batch_consumption, indirect_batch_consumption)
      
      
      # Select as number of matches the minimum of the needed and the possible
      max_batches <- min(consumption_per_batch$possible_batches) - (nrow(schedules)-i)
      batch_size <- dplyr::filter(technology, output == product, purpose == "production") %>%
        dplyr::select(output_standard_quantity) %>%
        unlist() %>% mean()
      need_batches <- ceiling(dplyr::filter(schedules, account == product)$production_needed[[1]] / batch_size)
      batch_number <- min(max_batches, need_batches)
      batches <- c(batches, batch_number)
      units <- c(units, batch_number * batch_size)
      
      # Compute the consumptions and remove them from what remain available
      consumption <- consumption_per_batch %>%
        dplyr::mutate(consumption = batch_consumption * batch_number) %>%
        dplyr::select(account,consumption)
      
      resources_available <- resources_available %>%
        dplyr::full_join(consumption, by = "account") %>%
        tidyr::replace_na(list(consumption = 0)) %>%
        dplyr::mutate(available = available - consumption) %>%
        dplyr::select(-consumption)
      
      consumptions[[product]] <- consumption
      
      rm(consumption, consumption_per_batch, need_batches, batch_number, batch_size, max_batches)
    }
    
    
    # Gather everything in a sales and production plan and purchases.
    
    schedules$batches_in_production <- batches
    schedules$units_in_production <- units
    schedules <- schedules %>%
      dplyr::mutate(units_available = beginning + units_in_production) %>%
      dplyr::mutate(sales = purrr::map2_dbl(units_available, demand, min)) %>%
      dplyr::select(account, batches = batches_in_production, production = units_in_production, sales)
    
    rm(batches, product, units, consumptions)
    
    
    activity <- split(technology, technology$purpose)
    
    activity$selling <- activity$selling %>%
      dplyr::mutate(account = as.numeric(stringr::str_replace_all(output, "400", "910"))) %>%
      dplyr::left_join(select(schedules, account, quantity = sales), by = "account") %>%
      dplyr::mutate(consumption = quantity / output_standard_quantity * input_standard_quantity) %>%
      dplyr::select(purpose, input, output, quantity = consumption, unit = input_unit)
    
    activity$production <- activity$production %>%
      dplyr::left_join(select(schedules, output = account, quantity = batches), by = "output") %>%
      dplyr::mutate(consumption = quantity * input_standard_quantity) %>%
      dplyr::select(purpose, input, output, quantity = consumption, unit = input_unit)
    
    prepsupport <- activity$production %>%
      dplyr::filter(input %in% activity$support$output) %>%
      dplyr::group_by(input) %>%
      dplyr::summarise(quantity = sum(quantity))
    
    activity$support <- activity$support %>%
      dplyr::left_join(select(prepsupport, output = input, quantity), by = "output") %>%
      dplyr::mutate(consumption = quantity / output_standard_quantity * input_standard_quantity) %>%
      dplyr::select(purpose, input, output, quantity = consumption, unit = input_unit)
    
    prepadmin1 <- activity$selling %>%
      dplyr::filter(input %in% activity$administration$output) %>%
      dplyr::group_by(input) %>%
      dplyr::summarise(quantity = sum(quantity))
    
    prepadmin2 <- activity$production %>%
      dplyr::filter(input %in% activity$administration$output) %>%
      dplyr::group_by(input) %>%
      dplyr::summarise(quantity = sum(quantity))
    
    prepadmin3 <- activity$support %>%
      dplyr::filter(input %in% activity$administration$output) %>%
      dplyr::group_by(input) %>%
      dplyr::summarise(quantity = sum(quantity))
    
    prepadmin <- prepadmin1 %>%
      dplyr::bind_rows(prepadmin2) %>%
      dplyr::bind_rows(prepadmin3) %>%
      dplyr::group_by(input) %>%
      dplyr::summarise(quantity = sum(quantity))
    
    activity$administration <- activity$administration %>%
      dplyr::left_join(select(prepadmin, output = input, quantity), by = "output") %>%
      dplyr::mutate(consumption = quantity / output_standard_quantity * input_standard_quantity) %>%
      dplyr::select(purpose, input, output, quantity = consumption, unit = input_unit)
    
    activity <- dplyr::bind_rows(activity)
    
    rm(prepadmin, prepadmin1, prepadmin2, prepadmin3, prepsupport, technology)
    
    
    
    # Gather schedules, activity, and available (unused) resources.
    schedules <- schedules %>%
      tidyr::pivot_longer(cols = c("batches","production","sales"), names_to = "unit", values_to = "quantity") %>%
      dplyr::mutate(input = dplyr::case_when(
        unit == "sales" ~ as.numeric(stringr::str_replace_all(account, "910", "131")),
        TRUE ~ account
      ))  %>%
      dplyr::mutate(output = dplyr::case_when(
        unit == "sales" ~ as.numeric(stringr::str_replace_all(input, "131", "500")),
        TRUE ~ as.numeric(stringr::str_replace_all(input, "910", "131"))
      )) %>%
      dplyr::mutate(purpose = dplyr::case_when(
        unit == "batches" ~ "inventory",
        unit == "production" ~ "inventory",
        TRUE ~ "customer"
      )) %>%
      dplyr::mutate(unit = dplyr::case_when(
        unit == "batches" ~ "batch",
        unit == "production" ~ "unit",
        TRUE ~ "unit"
      )) %>%
      dplyr::select(purpose, input, output, quantity, unit)
    
    
    # ensure that purchases have to be linked to capacity accounts
    
    preppurch <- activity %>%
      dplyr::mutate(account = dplyr::case_when(
        purpose == "selling" & input >= 21000 & input < 22000 ~ as.numeric(stringr::str_replace_all(output, "400", "612")),
        TRUE ~ input
      )) %>%
      dplyr::group_by(account, unit) %>%
      dplyr::summarise(consumed = sum(quantity)) %>%
      dplyr::full_join(resources_available, by = "account") %>%
      tidyr::replace_na(list(consumed = 0)) %>%
      dplyr::mutate(quantity = purrr::map2_dbl(minimum, consumed, max)) %>%
      dplyr::select(account, quantity, unit) %>%
      dplyr::ungroup()
    
    shipments <- competition[[company]]$capacity %>%
      dplyr::filter(nature == "services", purpose == "selling") %>%
      dplyr::select(purpose, origin, destination) %>%
      unique() %>%
      dplyr::mutate(account = destination, purpose = "purchases") %>%
      dplyr::left_join(preppurch, by = c("account")) %>%
      dplyr::ungroup() %>%
      dplyr::select(purpose, input = origin, output = destination, quantity, unit)
    
    salesemployment <- competition[[company]]$capacity %>%
      dplyr::filter(nature == "employment", purpose == "selling") %>%
      dplyr::select(nature, purpose, origin, destination) %>%
      dplyr::mutate(account = origin, purpose = "purchases") %>%
      unique() %>%
      dplyr::left_join(preppurch, by = "account") %>%
      dplyr::ungroup() %>%
      unique() %>%
      dplyr::select(purpose, input = origin, output = destination, quantity, unit)
    
    purchases <- competition[[company]]$capacity %>%
      dplyr::filter(nature %in% c("materials","services","employment"), purpose != "selling") %>%
      dplyr::select(nature, purpose, origin, destination) %>%
      dplyr::mutate(account = dplyr::case_when(
        nature == "materials" ~ destination,
        TRUE ~ origin
      ), purpose = "purchases") %>%
      unique() %>%
      dplyr::left_join(preppurch, by = "account") %>%
      dplyr::ungroup() %>%
      dplyr::select(purpose, input = origin, output = destination, quantity, unit)

    activity <- activity %>%
      dplyr::bind_rows(schedules) %>%
      dplyr::bind_rows(shipments) %>%
      dplyr::bind_rows(salesemployment) %>%
      dplyr::bind_rows(purchases) %>%
      dplyr::mutate(company = company, period = simperiod, quantity = round(quantity,2)) %>%
      dplyr::select(company, period, dplyr::everything()) %>%
      unique()
    
    rm(preppurch, shipments, salesemployment, purchases)
    
    
    usage <- activity %>%
      dplyr::ungroup() %>%
      dplyr::filter(unit != "batch") %>%
      dplyr::mutate(output = as.numeric(stringr::str_replace_all(output, "400", "612"))) %>%
      dplyr::mutate(purpose = dplyr::case_when(
        purpose == "purchases" ~ "purchased",
        purpose == "administration" ~ "consumed",
        purpose == "support" ~ "consumed",
        purpose == "production" ~ "consumed",
        purpose == "selling" ~ "consumed",
        purpose == "inventory" ~ "produced",
        purpose == "customer" ~ "sold",
        TRUE ~ "zzzz"
      )) %>%
      dplyr::mutate(account = dplyr::case_when(
        input %in% resources_available$account ~ input,
        TRUE ~ output
      )) %>%
      dplyr::group_by(company, period, account, purpose) %>%
      dplyr::summarise(quantity = sum(quantity, na.rm = TRUE)) %>%
      tidyr::pivot_wider(names_from = c("purpose"), values_from = "quantity", values_fill = list(quantity = 0)) %>%
      dplyr::left_join(dplyr::select(resources_available, account, beginning), by = "account") %>%
      dplyr::mutate(unused = beginning + produced + purchased - sold - consumed) %>%
      dplyr::select(company, period, account, beginning, purchased, consumed, produced, sold, unused)
    
    
    complete_profile <- schedules %>%
      dplyr::filter(purpose == "customer") %>%
      dplyr::mutate(
        company = company,
        period = simperiod,
        account = as.numeric(stringr::str_replace_all(input, "131", "400"))) %>%
      dplyr::select(company, period, account, tmp_sales = quantity) %>%
      dplyr::full_join( competition[[company]]$profile, by = c("company","period","account")) %>%
      dplyr::mutate(sales = dplyr::case_when(
        is.na(sales) ~ tmp_sales,
        TRUE ~ sales
      )) %>%
      dplyr::select(-tmp_sales)
    
    competition[[company]]$profile <- complete_profile
    competition[[company]]$activity <- unique(dplyr::filter(dplyr::bind_rows(competition[[company]]$activity, activity), period != ""))
    competition[[company]]$usage <- unique(dplyr::filter(dplyr::bind_rows(competition[[company]]$usage, usage), period != ""))
    
    rm(schedules, activity)
  }
  
  return(competition)
}
