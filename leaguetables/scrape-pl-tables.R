library(rvest)
library(tidyverse)

URL = 'https://fbref.com/en/comps/12/history/La-Liga-Seasons'
COUNTRYCODE = 'es'
COMPETITION = 'La Liga'

html = read_html(URL)
# write_html(html, 'leaguetables/tmp.html')

allszns = html %>%
  html_nodes('[data-stat="season"] a') %>% 
  map_dfr(~list(
    szn = .x %>% html_text(),
    url = .x %>% html_attr('href') %>% str_c('https://fbref.com', .))) %>% 
  mutate(rawhtml = map(url, read_html))

parsed = allszns %>% 
  mutate(
    table = map(rawhtml, ~.x %>% html_node('table.stats_table') %>% html_table()),
    teams = map_int(table, nrow),
    standings = map(table,
                    ~.x %>%
                      select(position = Rk, name = Squad, played = Apps,
                             points = Pts, difference = GDiff) %>% 
                      mutate(difference = as.character(difference))),
    cl = map(table,
             ~.x %>% 
               filter(str_detect(Notes, 'Champions League')) %>% 
               pull(Rk)),
    el = map(table,
             ~.x %>% 
               filter(str_detect(Notes, 'Europa League|UEFA Cup')) %>% 
               pull(Rk)),
    relegated = map(table,
                    ~.x %>% 
                      filter(str_detect(Notes, 'Relegat')) %>% 
                      pull(Rk)))

outputs = parsed %>% 
  mutate(competition = COMPETITION,
         code_string = COUNTRYCODE,
         year = as.integer(str_sub(szn, end = 4)),
         szn = str_c(str_sub(szn, end = 5), str_sub(szn, start = -2))) %>% 
  select(szn, year, competition, code_string, source = url, teams, cl, el, relegated, standings)

outputs

# l = outputs %>% 
#   select(competition:standings) %>% 
#   filter(row_number() == 2) %>% 
#   as.list()# %>% str()

formatjs <- function(s) {
  l = s %>% as.list()
  l$cl = l$cl[[1]]
  l$el = l$el[[1]]
  l$relegated = l$relegated[[1]]
  l$standings = l$standings[[1]]
  string = l %>% 
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>%
    as.character() %>% 
    str_split('\n') %>% 
    first()
  string[1] = str_c('var ', COUNTRYCODE, ' = ', string[1])
  str_c(string, collapse = '\n')
}

js = outputs %>% 
  group_by(szn, year) %>%
  nest() %>% 
  mutate(outputjs = map_chr(data, formatjs))

js
# cat(js$outputjs[2])

writejs <- function(szn, outputjs) {
  outputdir = str_c('leaguetables/', szn)
  if (!dir.exists(outputdir)) { dir.create(outputdir) }
  cat(outputjs, file = str_c(outputdir, '/', COUNTRYCODE, '.js'))
}

walk2(js$szn, js$outputjs, writejs)
