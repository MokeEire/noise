library(tidyverse)
library(here)

# Create normal distrib random forecasts
goodsell_forecasts = tibble(
  forecaster = 1:1000,
  forecast = rnorm(n = 1000, mean = .44, sd = .10)
)

write_csv(goodsell_forecasts, file = here("data", "goodsell_forecasts.csv"))
