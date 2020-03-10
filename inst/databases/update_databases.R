library(tidyverse)
library(tools)
library(stringr)
library(RefManageR)


cases <- tibble(case_list = list.files("inst/databases/cases")) %>%
  mutate(case_list = paste0("inst/databases/cases/", case_list)) %>%
  mutate(
    seasons = map(case_list, function(x,y) readxl::read_excel(x, sheet = y), y = 1),
    industries = map(case_list, function(x,y) readxl::read_excel(x, sheet = y), y = 2),
    families = map(case_list, function(x,y) readxl::read_excel(x, sheet = y), y = 3),
    products = map(case_list, function(x,y) readxl::read_excel(x, sheet = y), y = 4),
    activities = map(case_list, function(x,y) readxl::read_excel(x, sheet = y), y = 5),
    resources = map(case_list, function(x,y) readxl::read_excel(x, sheet = y), y = 6),
    consumptions = map(case_list, function(x,y) readxl::read_excel(x, sheet = y), y = 7),
    contracts = map(case_list, function(x,y) readxl::read_excel(x, sheet = y), y = 8)
  )


case_seasons <- cases$seasons %>%
  bind_rows()
save(case_seasons, file="data/case_seasons.RData")

case_industries <- cases$industries %>%
  bind_rows()
save(case_industries, file="data/case_industries.RData")

case_families <- cases$families %>%
  bind_rows()
save(case_families, file="data/case_families.RData")

case_products <- cases$products %>%
  bind_rows()
save(case_products, file="data/case_products.RData")

case_activities <- cases$activities %>%
  bind_rows()
save(case_activities, file="data/case_activities.RData")

case_resources <- cases$resources %>%
  bind_rows()
save(case_resources, file="data/case_resources.RData")

case_consumptions <- cases$consumptions %>%
  bind_rows()
save(case_consumptions, file="data/case_consumptions.RData")

case_contracts <- cases$contracts %>%
  bind_rows()
save(case_contracts, file="data/case_contracts.RData")

