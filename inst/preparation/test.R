library(tidyverse)

market <- simulacR::simul_market(
  start = "2020-01-01",
  years = 1,
  base_volume = 25,
  coeffday = simulacR::coeffday,
  coeffmonth = simulacR::coeffmonth
) %>%
  group_by(year, month) %>%
  summarise(market = sum(market))


items <- simulacR::products_services %>%
  left_join(simulacR::resources, by = c("resource","unit")) %>%
  filter(item %in% c("greek","parisian","italian")) %>%
  mutate(cost = consumption * price) %>%
  group_by(item) %>%
  summarise(cost = sum(cost))


offer <- data.frame(
  firm = c("firm1","firm1","firm1"),
  item = items$item,
  prices = c(10,10,10),
  qualities = items$cost,
  stringsAsFactors = FALSE
)


competition <- market %>%
  mutate(demand = map(market,
                      simulacR::simul_demand,
                      prices = offer$prices,
                      qualities = offer$qualities,
                      sensitivity = 0.3,
                      demand_function = "linear")) %>%
  mutate(demand = map(demand,
                      function(x, firm, item) data.frame(firm = firm, item = item, demand = x, stringsAsFactors = FALSE),
                      firm = offer$firm, item = offer$item)) %>%
  unnest(demand)

consumption <- competition %>%
  select(year, month, firm, item, quantity = demand) %>%
  left_join(simulacR::products_services, by = "item") %>%
  left_join(simulacR::resources, by = c("resource","unit")) %>%
  mutate(cost = quantity * consumption * price)






