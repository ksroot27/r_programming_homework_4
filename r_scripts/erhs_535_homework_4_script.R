library(tidyverse)
library(lubridate)

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

prop.test(x = baltimore$n_unsolved,
          n = baltimore$n_homicides)

