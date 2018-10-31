library(tidyverse)
library(broom)
library(purrr)
library(tibble)
library(scales)

homicides <- read_csv("data_raw/data-homicides-master/homicide-data.csv")
head(homicides)

homicides <- homicides %>% 
  unite(city_name,
        city,
        state,
        sep = ", ")

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
  mutate(test = map2(n_unsolved, n_homicides, 
                     ~ prop.test(.x, n = .y))) %>% 
  mutate(test = map(test,
                    ~ tidy(.x))) %>% 
  unnest(.drop = TRUE) %>% 
  select(city_name,
         estimate,
         conf.low,
         conf.high)
  
head(unsolved_prop)

unsolved_prop %>% 
  mutate(city_name = fct_reorder(city_name, estimate)) %>% 
  ggplot(aes(x = city_name,
             y = estimate)) +
  geom_point(color = "white") +
  geom_errorbar(color = "white", aes(x = city_name,
                                     ymin = conf.low,
                                     ymax = conf.high,
                                     width = 0)) +
  coord_flip() +
  labs(title = "Unsolved homicides by city",
       subtitle = "Bars show 95% confidence interval",
       x = NULL,
       y = "Percent of homicides that are unsolved") +
  scale_y_continuous(limits = c(.2, .75),
                     breaks = c(.2, .3, .4, .5, .6, .7),
                     labels = percent) +
  theme_dark()
  

  
