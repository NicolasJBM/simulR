
coeffday <- data.frame(
  weekday = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
  coeffday = c(0,1,1,1,1,1,0),
  stringsAsFactors = FALSE
)

save(coeffday, file = "data/coeffday.RData")

coeffmonth <- data.frame(
  month = c(1:12),
  coeffmonth = c(1,2,3,4,5,6,7,8,7,6,5,4),
  stringsAsFactors = FALSE
)

save(coeffmonth, file = "data/coeffmonth.RData")
