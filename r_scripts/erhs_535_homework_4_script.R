library(tidyverse)
library(lubridate)

homicides <- read_csv("data_raw/data-homicides-master/homicide-data.csv")
head(homicides)

homicides <- homicides %>% 
  unite(city_name, city, state, sep = ", ")

homicides$city_name

homicides %>% 
  mutate(unsolved = disposition == "Closed without arrest" | disposition == "Open/No arrest") %>% 
  group_by(city_name) %>% 
  summarize(n_homicides = n(),
            n_unsolved = count(unsolved))
  
  
levels(homicides$disposition)


  
  
  

  group_by(city_name)
