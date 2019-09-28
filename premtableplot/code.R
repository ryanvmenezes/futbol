library(rvest)
library(tidyverse)

url = 'https://fbref.com/en/comps/9/history/Premier-League-Seasons'

html = read_html(url)

getszn = function(u) {
  tbl = u %>% 
    read_html() %>% 
    html_nodes('table.stats_table') %>% 
    html_table()
  tbl[[1]]
}

sznurls = tibble(
  szn = html %>% html_nodes('[data-stat="season"] a') %>% html_text(),
  url = html %>% html_nodes('[data-stat="season"] a') %>% html_attr('href') %>% xml2::url_absolute('https://fbref.com/')
) 

allszns = sznurls %>% 
  mutate(table = map(url, getszn)) %>% 
  unnest() %>% 
  arrange(-points)

allszns %>% write_csv('premier-league-fbref.csv')

plot = allszns %>% 
  mutate(season = as.numeric(str_sub(szn, start=6))) %>% 
  ggplot(aes(season, Pts)) +
  geom_point() +
  geom_hline(
    yintercept = allszns %>% filter(Squad == 'Liverpool' & szn == '2018-2019') %>% pull(Pts)
  ) +
  theme_minimal()

plot

ggsave('plot.png', plot, device = 'png')
