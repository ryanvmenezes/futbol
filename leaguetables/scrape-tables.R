library(here)
library(rvest)
library(tidyverse)

leagues = tribble(
  ~competition, ~code_string, ~url,
  'Premier League','en','https://fbref.com/en/comps/9/history/Premier-League-Seasons',
  'La Liga','es','https://fbref.com/en/comps/12/history/La-Liga-Seasons',
  'Ligue 1','fr','https://fbref.com/en/comps/13/history/Ligue-1-Seasons',
  'Bundesliga','de','https://fbref.com/en/comps/20/history/Bundesliga-Seasons',
  'Serie A','it','https://fbref.com/en/comps/11/history/Serie-A-Seasons'
)


getorretrieve = function(url) {
  fname = url %>% 
    str_split('/') %>% 
    `[[`(1) %>% 
    `[`(length(.)) %>% 
    str_c('.html')
  
  fpath = here('leaguetables','raw', fname)
  
  if (file.exists(fpath)) {
    h = read_html(fpath)
  } else {
    h = read_html(url)
    write_html(h, fpath)
  }
  
  h
}

historyhtml = leagues %>% 
  mutate(html = map(url, getorretrieve)) 
  
szns = historyhtml %>% 
  mutate(sznshtml = map(html, ~.x %>% html_nodes('[data-stat="season"] a')),
         szn = map(sznshtml, ~.x %>% html_text()),
         source = map(sznshtml, ~.x %>% html_attr('href') %>% str_c('https://fbref.com', .)))

sznshtml = szns %>% 
  select(-url, -html,-sznshtml) %>% 
  unnest() %>% 
  mutate(html = map(source, getorretrieve))

sznshtml

parsed = sznshtml %>% 
  mutate(
    table = map(html, ~.x %>% html_node('table.stats_table') %>% html_table()),
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
                      pull(Rk)),
    # year = as.integer(str_sub(szn, end = 4)),
    szn = str_c(str_sub(szn, end = 5), str_sub(szn, start = -2))
  ) %>%
  select(szn, competition, code_string, source, teams, cl, el, relegated, standings)

parsed

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
  string[1] = str_c('var ', s$code_string, ' = ', string[1])
  str_c(string, collapse = '\n')
}

js = parsed %>% 
  mutate(code2 = code_string) %>% 
  group_by(szn, code2) %>% 
  nest() %>% 
  mutate(outputjs = map_chr(data, formatjs)) %>% 
  select(-data)

js
# cat(js$outputjs[2])

writejs <- function(szn, code2, outputjs) {
  outputdir = here('leaguetables', 'js', szn)
  if (!dir.exists(outputdir)) { dir.create(outputdir) }
  fname = str_c(code2, '.js')
  outputfile = here('leaguetables', 'js', szn, outputfname)
  cat(outputjs, file = outputfile)
}

pwalk(js, writejs)
