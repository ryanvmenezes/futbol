library(rvest)
library(lubridate)
library(tidyverse)

ratings.raw = read_csv('ratings-uncleaned.csv')

ratings.raw

comps = read_csv('competitions-classified.csv')

comps

ratings.grouped = comps %>% 
  select(comp, overallgroup) %>% 
  right_join(ratings.raw) %>% 
  arrange(-viewers) %>% 
  mutate(
    overallgroup = replace_na(overallgroup, 'other'),
    overallgroup = fct_relevel(overallgroup, 'mworldcup18', 'wworldcup19')
  )

ratings.grouped %>% 
  mutate(gmmonth = floor_date(gmdate, unit = 'months')) %>% 
  group_by(overallgroup, gmmonth) %>% 
  summarise(viewers = sum(viewers)) %>% 
  ggplot(aes(gmmonth, viewers, fill = overallgroup)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal()
