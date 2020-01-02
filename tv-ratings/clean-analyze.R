library(lubridate)
library(tidyverse)

raw.ratings = read_csv('ratings-uncleaned.csv')

raw.ratings

raw.ratings %>% arrange(-viewers)

date.df = raw.ratings %>% 
  distinct(rangestart, rangeend) %>% 
  mutate(
    dates = map2(
      rangestart,
      rangeend,
      ~seq(.x, .y, by = '1 day')
    )
  ) %>% 
  unnest(cols = c(dates)) %>% 
  count(dates) %>% 
  mutate(
    yrmo = floor_date(dates, unit = 'months'),
    yr = year(dates),
    mo = month(dates, label = TRUE),
    wd = wday(dates, label = TRUE),
    wk = week(dates),
  ) %>% 
  group_by(yrmo) %>% 
  mutate(mowk = wk - min(wk) + 1)

date.df %>% 
  ggplot(aes(mowk, wd, fill = n)) +
  geom_tile(colour = 'white') +
  facet_grid(yr ~ mo) +
  theme_minimal()

# ggplot(dat, aes(monthweek, weekdayf, fill = VIX.Close)) + 
#   geom_tile(colour = "white") +
#   facet_grid(year~monthf) +
#   scale_fill_gradient(low="red", high="yellow") +
#   opts(title = "Time-Series Calendar Heatmap") +
#   xlab("Week of Month") +
#   ylab("")
# P


# comps = raw.ratings %>%
#   count(comp) %>% 
#   arrange(-n)
# 
# comps
# 
# comps %>% write_csv('ratings-comps.csv')

comps

## get comps from google sheet

rawgames %>% filter(comp == 'Championship')
