library(lubridate)
library(tidyverse)
library(googlesheets4)

raw.ratings = read_csv('ratings-uncleaned.csv')

raw.ratings

raw.ratings %>% arrange(-viewers)

comps = raw.ratings %>% 
  group_by(comp) %>% 
  summarise(
    countgames = n(),
    totalviewers = sum(viewers),
    avgviewers = totalviewers / countgames
  )

comps

SHEET = '1JjO7m1qCtzPTDSltUiBDqX6b9Mfki-cssrirE5CsLJY'
sheetname = 'organized'

comps.classified = read_sheet(SHEET, sheet = sheetname)

comps.classified

comps.classified %>% write_csv('competitions-classified.csv')

new.comps = comps %>% 
  left_join(comps.classified %>% select(-countgames, -totalviewers, -avgviewers)) %>% 
  arrange(continent, country, clubcountry, womens)

new.comps

new.comps %>% filter(is.na(continent))

new.comps %>% 
  write_sheet(SHEET, sheet = sheetname)

# tk: teams, networks
