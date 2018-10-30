library(tidyverse)
library(broom)

homicides <- read_csv("data_raw/data-homicides-master/homicide-data.csv")
head(homicides)

homicides <- homicides %>% 
  unite(city_name, city, state, sep = ", ")

homicides$city_name

unique(homicides$disposition)

unsolved <- homicides %>% 
  mutate(unsolved = disposition == "Closed without arrest" | disposition == "Open/No arrest") %>% 
  group_by(city_name) %>% 
  summarize(n_homicides = n(),
            n_unsolved = sum(unsolved))

baltimore <- unsolved %>% 
  filter(city_name == "Baltimore, MD")

baltimore_prop <- prop.test(x = baltimore$n_unsolved,
          n = baltimore$n_homicides)

tidy(baltimore_prop) %>% 
  select(estimate, conf.low, conf.high)
