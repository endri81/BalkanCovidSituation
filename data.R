
library(covidregionaldata)
library(tidyverse)
library(scales)
g7_nots <- get_national_data(countries = c('Slovenia', 'Croatia', 'Bosnia and Herzegovina', 'Serbia', 'Montenegro', 'Albania', 'Macedonia', 'Greece', 'Bulgaria', 'Romania', 'Kosovo'), verbose = FALSE)


g7_nots1 <- g7_nots %>% select(country,cases_new, cases_total, deaths_new, deaths_total, date) %>% arrange(desc(date))

population <-c(2.838, 3.281,6.927, 4.047, 10.72, 1.873, 0.621, 2.083, 19.29, 6.908,2.1)

deathpermilion <-g7_nots1[c(1:11) , ] %>% mutate(population = population) %>% 
mutate(deathpermilion = deaths_new/population)  %>% 
filter(deathpermilion == max(deathpermilion)) %>% 
select(country, deathpermilion) %>%
mutate_if(is.numeric, round,2)


infpermilion <-g7_nots1[c(1:11) , ] %>% mutate(population = population) %>% 
  mutate(casespermilion = cases_new/population)  %>% 
  filter(casespermilion == max(casespermilion)) %>% 
  select(country, casespermilion) %>%
  mutate_if(is.numeric, round,2)

mostdeath <-g7_nots %>% group_by(country)  %>% 
  summarise(mostdeath = sum(deaths_new))  %>% 
  mutate(population = population) %>%
  mutate(mostdeathpop = mostdeath/population)  %>%
  filter(mostdeath == max(mostdeath)) %>% 
  select(country, mostdeath) %>%
  mutate_if(is.numeric, round,2)


deaths <-g7_nots %>% group_by(country)  %>% 
  summarise(mostdeath = sum(deaths_new))  %>% 
  mutate(population = population) %>%
  mutate(mostdeathpop = mostdeath/population)  %>%
  mutate_if(is.numeric, round,2)


cases <- g7_nots1[c(1:11) , ] %>% 
  select(country, cases_new) %>%
  mutate_if(is.numeric, round,2)

  