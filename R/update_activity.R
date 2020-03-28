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
      dplyr::mutate(available =  quantity * capacity * simworkdays) %>%
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
      dplyr::select(account, priority, demand, production_needed) %>%
      dplyr::arrange(priority) %>%
      dplyr::mutate(account = as.numeric(stringr::str_replace_all(account, "131", "910")))
    
    rm(demand_plan)
    
    # simulate the planned production and the related consumption of resources, in order of priority, until one resource is missing
    
    batches <- c()
    units <- c()
    consumptions <- list()
    
    for (product in schedules$account){
      
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
      max_batches <- min(consumption_per_batch$possible_batches)
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
      dplyr::mutate(sales = purrr::map2_dbl(units_in_production, demand, min)) %>%
      dplyr::select(account, batches = batches_in_production, production = units_in_production, sales)
    
    rm(batches, product, units, consumptions)
    
    
    activity <- split(technology, technology$purpose)
    
    activity$selling <- activity$selling %>%
      dplyr::mutate(account = as.numeric(stringr::str_replace_all(output, "612", "910"))) %>%
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
    
    activity <- dplyr::bind_rows(activity)
    
    rm(prepsupport, technology)
    
    
    
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
        unit == "batches" ~ "production",
        unit == "production" ~ "production",
        TRUE ~ "selling"
      )) %>%
      dplyr::mutate(unit = dplyr::case_when(
        unit == "batches" ~ "batch",
        unit == "production" ~ "unit",
        TRUE ~ "unit"
      )) %>%
      dplyr::select(purpose, input, output, quantity, unit)
    
    
    # ensure that purchases have to be linked to capacity accounts
    
    preppurch <- activity %>%
      dplyr::mutate(destination = dplyr::case_when(
        purpose == "selling" ~ output,
        TRUE ~ input
      )) %>%
      dplyr::group_by(account = input, destination, unit) %>%
      dplyr::summarise(consumed = sum(quantity)) %>%
      dplyr::full_join(resources_available, by = "account") %>%
      tidyr::replace_na(list(consumed = 0)) %>%
      dplyr::mutate(quantity = purrr::map2_dbl(minimum, consumed, max)) %>%
      dplyr::select(account, destination, quantity, unit) %>%
      dplyr::ungroup()
    
    shipments <- competition[[company]]$capacity %>%
      dplyr::filter(nature == "services", purpose == "selling") %>%
      dplyr::select(purpose, origin, destination) %>%
      unique() %>%
      dplyr::mutate(account = origin, purpose = "purchases") %>%
      dplyr::left_join(preppurch, by = c("account", "destination")) %>%
      dplyr::ungroup() %>%
      dplyr::select(purpose, input = origin, output = destination, quantity, unit)
    
    purchases <- competition[[company]]$capacity %>%
      dplyr::filter(nature %in% c("materials","services","employment"), purpose != "selling") %>%
      dplyr::select(nature, purpose, origin, destination) %>%
      dplyr::mutate(account = dplyr::case_when(
        nature == "materials" ~ destination,
        TRUE ~ origin
      ), purpose = "purchases") %>%
      unique() %>%
      dplyr::left_join(dplyr::select(preppurch, -destination), by = "account") %>%
      dplyr::ungroup() %>%
      dplyr::select(purpose, input = origin, output = destination, quantity, unit)
    
    
    rm(preppurch)
    
    
    activity <- activity %>%
      dplyr::bind_rows(schedules) %>%
      dplyr::bind_rows(shipments) %>%
      dplyr::bind_rows(purchases) %>%
      dplyr::mutate(company = company, period = simperiod) %>%
      dplyr::select(company, period, dplyr::everything()) %>%
      unique()
    
    competition[[company]]$activity <- unique(dplyr::filter(dplyr::bind_rows(competition[[company]]$activity, activity), period != ""))
    
    rm(schedules, shipments, purchases, activity)
  }
  
  return(competition)
}
