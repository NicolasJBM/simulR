


update_activity <- function(simperiod, competition){
  
  
  for (company in names(competition)){
    
  
    
    
    
    company <- names(competition)[1]
    simperiod = periodic_demand$period[[period_nbr]]
    simworkdays = periodic_demand$working_day[[period_nbr]]
    
    
    
    
    
    
    ##########################################################################################################
    # Gather information about inventories, purchases and production
    
    beginning <- list()
    
    beginning$finpro <- competition[[company]]$census$finished_products %>%
      dplyr::select(resource, available = quantity)
    beginning$rawmat <- competition[[company]]$census$raw_materials %>%
      dplyr::select(resource, available = quantity)
    beginning$assets <- competition[[company]]$census$assets %>%
      dplyr::mutate(available = capacity * simworkdays) %>%
      dplyr::select(resource = resource, available)
    
    beginning <- dplyr::bind_rows(beginning) %>%
      dplyr::select(resource, initial = available)
    
    
    
    obtainable <- list()
    
    obtainable$labor <- competition[[company]]$capacity %>%
      dplyr::filter(contract == "employment") %>%
      dplyr::select(resource, parameter, value) %>%
      tidyr::pivot_wider(names_from = c("parameter"), values_from = "value") %>%
      dplyr::mutate(available =  quantity * capacity * simworkdays) %>%
      dplyr::mutate(
        minimum = available,
        maximum = available,
      ) %>%
      dplyr::select(resource, minimum, maximum)
    
    obtainable$purchases <- competition[[company]]$capacity %>%
      dplyr::filter(contract == "purchase") %>%
      dplyr::select(resource, parameter, value) %>%
      tidyr::pivot_wider(names_from = c("parameter"), values_from = "value") %>%
      dplyr::mutate(available = quantity) %>%
      dplyr::mutate(
        minimum = available * (1-flexibility),
        maximum = available * (1+flexibility),
      ) %>%
      dplyr::select(resource, minimum, maximum) %>%
      na.omit()
    
    obtainable$production <- competition[[company]]$capacity %>%
      dplyr::filter(contract == "production") %>%
      dplyr::select(resource, parameter, value) %>%
      tidyr::pivot_wider(names_from = c("parameter"), values_from = "value") %>%
      dplyr::mutate(available = quantity) %>%
      dplyr::mutate(
        minimum = 0,
        maximum = available,
      ) %>%
      dplyr::select(resource, minimum, maximum) %>%
      na.omit()
    
    obtainable <- dplyr::bind_rows(obtainable)
    
    resources_available <- beginning %>%
      dplyr::full_join(obtainable, by = "resource") %>%
      tidyr::replace_na(list(initial = 0, minimum = 0, maximum = 0)) %>%
      dplyr::mutate(minimum = minimum + initial, maximum = maximum + initial) %>%
      dplyr::group_by(resource) %>%
      dplyr::summarise(initial = sum(initial), minimum = sum(minimum), maximum = sum(maximum)) %>%
      dplyr::mutate(available = maximum)
    
    rm(beginning, obtainable)
    
    
    ##########################################################################################################
    
    
    technology <- competition[[company]]$technology %>%
      dplyr::select(phase, nature, destination, costing, output, resource = input, output_standard_quantity, resource_consumed = input_standard_quantity)
    
    demand_plan <- competition[[company]]$profile %>%
      dplyr::filter(period == simperiod) %>%
      dplyr::select(resource, demand) %>%
      dplyr::mutate(plan = purrr::map_dbl(resource, function(x,y) dplyr::filter(y, resource == x)$available[[1]], y = resources_available))
    
    sales_production_plan <- competition[[company]]$capacity %>%
      dplyr::filter(contract == "production", parameter == "priority") %>%
      dplyr::select(resource, priority = value) %>%
      dplyr::left_join(demand_plan, by = "resource") %>%
      dplyr::left_join(dplyr::select(resources_available, resource, initial), by = "resource") %>%
      dplyr::mutate(needed = purrr::map2_dbl(demand, plan, max)) %>%
      dplyr::mutate(production_needed = needed - initial) %>%
      dplyr::select(resource, priority, production_needed) %>%
      dplyr::arrange(priority)
    
    rm(demand_plan)
    
    # simulate the planned production and the related consumption of resources, in order of priority, until one resource is missing
    
    batches <- c()
    units <- c()
    consumptions <- list()
    
    for (product in sales_production_plan$resource){
      
      # First, compte resources directly and indirectly consumed by every batch
      direct_batch_consumption <- technology %>%
        dplyr::filter(phase == "production", output == product) %>%
        dplyr::select(resource, batch_consumption = resource_consumed)
      
      indirect_batch_consumption <- technology %>%
        dplyr::filter(phase == "support", output %in% direct_batch_consumption$resource) %>%
        dplyr::left_join(dplyr::select(direct_batch_consumption, output = resource, batch_consumption), by = "output") %>%
        dplyr::mutate(batch_consumption = batch_consumption * resource_consumed / output_standard_quantity) %>%
        dplyr::select(resource, batch_consumption)
      
      consumption_per_batch <- direct_batch_consumption %>%
        dplyr::bind_rows(indirect_batch_consumption) %>%
        dplyr::group_by(resource) %>%
        dplyr::summarise(batch_consumption = sum(batch_consumption)) %>%
        dplyr::left_join(dplyr::select(resources_available, resource, available), by = "resource") %>%
        dplyr::mutate(possible_batches = floor(available / batch_consumption))
      
      rm(direct_batch_consumption, indirect_batch_consumption)
      
      
      # Select as number of matches the minimum of the needed and the possible
      max_batches <- min(consumption_per_batch$possible_batches)
      batch_size <- dplyr::filter(technology, output == product, phase == "production") %>%
        dplyr::select(output_standard_quantity) %>%
        unlist() %>% mean()
      need_batches <- ceiling(dplyr::filter(sales_production_plan, resource == product)$production_needed[[1]] / batch_size)
      batch_number <- min(max_batches, need_batches)
      batches <- c(batches, batch_number)
      units <- c(units, batch_number * batch_size)
      
      # Compute the consumptions and remove them from what remain available
      consumption <- consumption_per_batch %>%
        dplyr::mutate(consumption = batch_consumption * batch_number) %>%
        dplyr::select(resource,consumption)
      
      resources_available <- resources_available %>%
        dplyr::left_join(consumption, by = "resource") %>%
        tidyr::replace_na(list(consumption = 0)) %>%
        dplyr::mutate(available = available - consumption) %>%
        dplyr::select(-consumption)
      
      consumptions[[product]] <- consumption %>%
        dplyr::select(resource, consumption)
      
      rm(consumption, consumption_per_batch, need_batches, batch_number, batch_size, max_batches)
    }
    
    
    # Gather everything in a sales and production plan and purchases.
    
    sales_production_plan$batches_in_production <- batches
    sales_production_plan$units_in_production <- units
    sales_production_plan <- sales_production_plan %>%
      dplyr::mutate(sales = purrr::map2_dbl(production_needed, units_in_production, min)) %>%
      dplyr::select(resource, batches = batches_in_production, production = units_in_production, sales)
    
    sales_consumptions <- technology %>%
      dplyr::filter(phase == "sale") %>%
      dplyr::left_join(dplyr::select(sales_production_plan, output = resource, sales), by = "output") %>%
      dplyr::mutate(consumption = sales * resource_consumed / output_standard_quantity) %>%
      dplyr::select(resource, consumption) %>%
      dplyr::group_by(resource) %>%
      dplyr::summarise(consumption = sum(consumption))
    
    consumptions <- dplyr::bind_rows(consumptions) %>%
      dplyr::group_by(resource) %>%
      dplyr::summarise(consumption = sum(consumption)) %>%
      dplyr::bind_rows(sales_consumptions)
    
    purchases <- resources_available %>%
      dplyr::mutate(nedded = maximum - available) %>%
      dplyr::mutate(purchase = purrr::map2_dbl(minimum, nedded, max)) %>%
      dplyr::filter(resource %in% unique(dplyr::filter(competition[[company]]$capacity, contract == "purchase")$resource)) %>%
      dplyr::filter(purchase > 0) %>%
      dplyr::select(resource, purchase) %>%
      dplyr::left_join(consumptions, by = "resource")
    
    rm(consumptions, sales_consumptions, resources_available, batches, product, units)
    
    
    
    
    
    
    
    activity <- split(technology, technology$phase)
    
    activity$sale <- activity$sale %>%
      dplyr::left_join(select(sales_production_plan, output = resource, quantity = sales), by = "output") %>%
      dplyr::mutate(consumption = quantity / output_standard_quantity * resource_consumed) %>%
      dplyr::select(costing, nature, resource, destination, output, quantity = consumption)
    
    activity$production <- activity$production %>%
      dplyr::left_join(select(sales_production_plan, output = resource, quantity = batches), by = "output") %>%
      dplyr::mutate(consumption = quantity * resource_consumed) %>%
      dplyr::select(costing, nature, resource, destination, output, quantity = consumption)
    
    prepsupport <- activity$production %>%
      dplyr::group_by(resource) %>%
      dplyr::summarise(quantity = sum(quantity))
    
    activity$support <- activity$support %>%
      dplyr::left_join(select(prepsupport, output = resource, quantity), by = "output") %>%
      dplyr::mutate(consumption = quantity / output_standard_quantity * resource_consumed) %>%
      dplyr::select(costing, nature, destination, resource, quantity = consumption)
    
  
    activity <- dplyr::bind_rows(activity)
    
    select(competition[[company]]$costing, nature = allocation_base, account, assignment) %>%
      inner_join(activity, by = "nature")
    
  }
  
  
  return(competition)
}