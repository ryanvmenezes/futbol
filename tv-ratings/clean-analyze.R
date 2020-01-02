library(lubridate)
library(tidyverse)

rawgames = read_csv('ratings-uncleaned.csv')
rawgames

comps = rawgames %>%
  count(comp) %>% 
  arrange(-n)

comps %>% write_csv('ratings-comps.csv')
rawgames %>% filter(comp == 'Championship')
