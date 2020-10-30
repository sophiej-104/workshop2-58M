library(tidyverse)

hdi <- read_csv("data-raw/Human-development-index.csv")

hdi_tidy <- hdi %>%
  pivot_longer(names_to = "Year",
               values_to = "HDI_Value",
               cols = -c(1:2))
