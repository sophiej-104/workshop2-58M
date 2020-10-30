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

# Filter the summary to get just the ten countries with the lowest mean HDI using:

hdi_summary_low <- hdi_summary %>% 
  filter(rank(mean_index) < 11)

hdi_summary_low

# Plotting

hdi_summary_low %>% 
  ggplot() +
  geom_point(aes(x = Country,
                 y = mean_index)) +
  geom_errorbar(aes(x = Country,
                    ymin = mean_index - se_index,
                    ymax = mean_index + se_index)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI Value") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

hdi_tidy %>% 
  group_by(Country) %>% 
  summarise(mean_index = mean(HDI_Value),
            n = length(HDI_Value),
            sd_index = sd(HDI_Value),
            se_index = sd(HDI_Value)/sqrt(n())) %>%
  ggplot() +
  geom_point(aes(x = Country,
                 y = mean_index)) +
  geom_errorbar(aes(x = Country,
                    ymin = mean_index - se_index,
                    ymax = mean_index + se_index)) +
  scale_y_continuous(expand = c(0, 0),
                     name = "HDI Value") +
  scale_x_discrete(expand = c(0, 0),
                   name = "Countries",
                   guide = guide_axis(check.overlap = T)) +
  theme_classic() +
  coord_flip()
