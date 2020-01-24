products_services <- list(
    data.frame(
      industry = "hospitality",
      family = "food and beverages",
      subfamily = "food",
      type = "paninis",
      item = "parisian",
      resource =    c("bread","butter","ham"  ,"labor_low" ,"oven"  ,"bag"),
      unit =        c("gram" ,"gram"  ,"gram" ,"minute","minute","piece"),
      consumption = c(150    ,15      ,60     ,1       ,2       ,1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      industry = "hospitality",
      family = "food and beverages",
      subfamily = "food",
      type = "paninis",
      item = "italian",
      resource =    c("bread","grana","serrano","labor_low","oven"  ,"bag"),
      unit =        c("gram" ,"gram" ,"gram"  ,"minute","minute","piece"),
      consumption = c(150    ,20     ,40      ,1.5     ,1.5     ,1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      industry = "hospitality",
      family = "food and beverages",
      subfamily = "food",
      type = "paninis",
      item = "greek",
      resource =    c("bread","feta","honey","labor_low" ,"oven"  ,"bag"),
      unit =        c("gram" ,"gram","gram" ,"minute","minute","piece"),
      consumption = c(150    ,50    ,30     ,1.5     ,1       ,1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      industry = "hospitality",
      family = "food and beverages",
      subfamily = "food",
      type = "salads",
      item = "thai",
      resource =    c("mango","concumber","carrot","labor_low" ,"houder"),
      unit =        c("gram" ,"gram"     ,"gram"  ,"minute","piece"),
      consumption = c(80     ,80         ,80      ,8       ,1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      industry = "hospitality",
      family = "food and beverages",
      subfamily = "food",
      type = "salads",
      item = "perigourdine",
      resource =    c("duck","potatoes","salad","labor_low" ,"houder"),
      unit =        c("gram","gram"    ,"gram" ,"minute","piece"),
      consumption = c(75    , 75       ,100    ,3       ,1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      industry = "hospitality",
      family = "food and beverages",
      subfamily = "beverages",
      type = "cocktails",
      item = "mojito",
      resource =    c("rum"        ,"soda"        ,"mint","lime" ,"labor_low" ,"cup"),
      unit =        c("centiliter" ,"centiliter"  ,"gram","piece","minute","piece"),
      consumption = c(5            ,20            ,5     ,1      ,0.75    ,1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      industry = "hospitality",
      family = "food and beverages",
      subfamily = "beverages",
      type = "cocktails",
      item = "punch",
      resource =    c("rum"        ,"juice"     ,"labor_low" ,"cup"),
      unit =        c("centiliter" ,"centiliter","minute","piece"),
      consumption = c(7            ,18          ,0.1     ,1),
      stringsAsFactors = FALSE
    ),
    cuba_libre = data.frame(
      industry = "hospitality",
      family = "food and beverages",
      subfamily = "beverages",
      type = "cocktails",
      item = "cuba libre",
      resource =    c("rum"       ,"cola"      ,"lime" ,"labor_low" ,"cup"),
      unit =        c("centiliter","centiliter","piece","minute","piece"),
      consumption = c(5           ,20          ,1      ,0.5     ,1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      industry = "hospitality",
      family = "food and beverages",
      subfamily = "beverages",
      type = "soft drinks",
      item = "soda",
      resource =    c("soda"      ,"labor_low" ,"cup"),
      unit =        c("centiliter","minute","piece"),
      consumption = c(30          , 0.10   ,1),
      stringsAsFactors = FALSE
    ),
    juice = data.frame(
      industry = "hospitality",
      family = "food and beverages",
      subfamily = "beverages",
      type = "soft drinks",
      item = "juice",
      resource =    c("juice"     ,"labor_low" ,"cup"),
      unit =        c("centiliter","minute","piece"),
      consumption = c(30          , 0.10   ,1),
      stringsAsFactors = FALSE
    ),
    cola = data.frame(
      industry = "hospitality",
      family = "food and beverages",
      subfamily = "beverages",
      type = "soft drinks",
      item = "cola",
      resource =    c("cola"      ,"labor_low" ,"cup"),
      unit =        c("centiliter","minute","piece"),
      consumption = c(30          , 0.10   ,1),
      stringsAsFactors = FALSE
    )
) %>%
  dplyr::bind_rows()

save(products_services, file = "data/products_services.RData")


resources <- data.frame(
  resource = c(
    "bread",
    "butter",
    "ham",
    "labor_low",
    "oven",
    "bag",
    "grana",
    "serrano",
    "feta",
    "honey",
    "mango",
    "concumber",
    "carrot",
    "houder",
    "duck",
    "potatoes",
    "salad",
    "rum",
    "soda",
    "mint",
    "lime",
    "cup",
    "juice",
    "cola"
  ),
  nature = c(
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "labor",
    "equipment",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials",
    "raw_materials"
  ),
  unit = c(
    "gram",
    "gram",
    "gram",
    "minute",
    "minute",
    "piece",
    "gram",
    "gram",
    "gram",
    "gram",
    "gram",
    "gram",
    "gram",
    "piece",
    "gram",
    "gram",
    "gram",
    "centiliter",
    "centiliter",
    "gram",
    "piece",
    "piece",
    "centiliter",
    "centiliter"
  ),
  price = c(
    0.02,
    0.01,
    0.03,
    1,
    0.04,
    0.1,
    0.02,
    0.04,
    0.02,
    0.02,
    0.006,
    0.004,
    0.002,
    0.15,
    0.1,
    0.01,
    0.003,
    0.2,
    0.002,
    0.05,
    0.3,
    0.1,
    0.03,
    0.02
  ),
  stringsAsFactors = FALSE
)

save(resources, file = "data/resources.RData")



cost <- products_services %>%
  left_join(resources, by = c("resource", "unit")) %>%
  mutate(cost = consumption * price) %>%
  group_by(nature, item) %>%
  summarise(cost = sum(cost))


