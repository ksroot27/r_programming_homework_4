library(tidyverse)
library(broom)
library(purrr)
library(tibble)

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

unsolved_prop <- unsolved %>% 
  mutate(test = map2(n_unsolved, n_homicides,  ~ prop.test(.x, n = .y))) %>% 
  mutate(test = map(test, ~ tidy(.x))) %>% 
  unnest(.drop = TRUE) %>% 
  select(city_name, estimate, conf.low, conf.high)
  
head(unsolved_prop)

unsolved_prop %>% 
  mutate(city_name = fct_reorder(city_name, estimate)) %>% 
  ggplot() +
  geom_point(aes(x = city_name, y = estimate)) +
  geom_errorbar(aes(x = city_name, ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  theme_dark()

  
