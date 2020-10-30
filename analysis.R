library(tidyverse)

#Importing data and reformatting to be more 'tidy'

hdi <- read_csv("data-raw/Human-development-index.csv")

hdi_tidy <- hdi %>%
  pivot_longer(names_to = "Year",
               values_to = "HDI_Value",
               cols = -c(1:2))

# Checking and removing missing values

hdi_no_na <- hdi_tidy %>%
  filter(HDI_Value != "NA")

# Summarising the data 

hdi_summary <- hdi_no_na %>% 
  group_by(Country) %>% 
  summarise(mean_index = mean(HDI_Value),
            n = length(HDI_Value),
            sd_index = sd(HDI_Value),
                          se_index = sd(HDI_Value)/sqrt(n()))
