library(here)
library(rvest)
library(lubridate)
library(tidyverse)

posts = read_csv('posts.csv')

posts

mw.posts = posts %>% 
  filter(str_detect(title, 'Most-watched')) %>% 
  arrange(desc(link))

mw.posts

getorretrieve = function(url, override = FALSE) {
  fname = url %>% 
    str_split('/') %>% 
    `[[`(1) %>% 
    `[`(length(.) - 1) %>%
    str_c('.html')
  
  fpath = here::here('most-watched-index', fname)
  
  if (!override & file.exists(fpath)) {
    h = read_html(fpath)
  } else {
    h = read_html(url)
    write_html(h, fpath)
  }
  
  h
}

pages = mw.posts %>% 
  mutate(rawhtml = map(link, getorretrieve))

pages

parsed = pages %>% 
  mutate(
    tbl = map(
      rawhtml,
      function(.x) {
        tbls = .x %>% html_table()
        if (length(tbls) > 0) {
          return(tbls[[1]])
        }
      }
    )
  ) %>% 
  filter(map_int(tbl, length) > 0) %>% 
  mutate(
    dfr = map_int(tbl, nrow),
    dfc = map_int(tbl, ncol),
    cnames = map_chr(tbl, ~.x %>% names() %>% str_c(collapse = '|'))
  ) %>% 
  select(-link)

parsed

post17 = parsed %>% 
  mutate(postdate = mdy(postdate)) %>% 
  filter(postdate > ymd('2017-01-06')) %>% 
  filter(str_detect(cnames, 'Compe'))

post17

post17 %>% count(cnames) %>% arrange(-n)

allgames = post17 %>% 
  mutate(
    cleaneddf = map2(
      tbl,
      cnames,
      function(.x, .y) {
        final = NULL
        if (.y == 'Rank|Date|Competition|Teams|TV1|TV2|Total') {
          final = .x %>% 
            unite(network, TV1:TV2, sep = ' / ') %>% 
            select(gmrank = Rank, gmdate = Date, comp = Competition, teams = Teams, network, viewers = Total)
        }
        if (.y == 'Rank|Date|Competition|Teams|TV1|Total') {
          final = .x %>% 
            select(gmrank = Rank, gmdate = Date, comp = Competition, teams = Teams, network = TV1, viewers = Total)
        }
        if (.y == 'Rank|Date|Competition|Teams|Network|Total') {
          final = .x %>% 
            select(gmrank = Rank, gmdate = Date, comp = Competition, teams = Teams, network = Network, viewers = Total)
        }
        if (.y == 'Rank|Home|Away|Competition|Date|TV1|TV2|Total') {
          final = .x %>% 
            unite(teams, Home:Away, sep = ' / ') %>% 
            unite(network, TV1:TV2, sep = ' / ') %>% 
            select(gmrank = Rank, gmdate = Date, comp = Competition, teams, network, viewers = Total)
        }
        if (.y == 'Rank|Date|Competition|Teams|TV') {
          final = .x %>% 
            mutate(viewers = str_replace(TV, '.*; ', '')) %>% 
            select(gmrank = Rank, gmdate = Date, comp = Competition, teams = Teams, network = TV, viewers)
        }
        if (.y == 'Rank|Date|Competition|Teams|TV|Column1') {
          final = .x %>% 
            select(gmrank = Rank, gmdate = Date, comp = Competition, teams = Teams, network = TV, viewers = Column1)
        }
        if (.y == 'Rank|Date|Competition|Teams|TV|Viewership') {
          final = .x %>% 
            select(gmrank = Rank, gmdate = Date, comp = Competition, teams = Teams, network = TV, viewers = Viewership)
        }
        if (.y == 'Rank|Teams|Competition|Date|TV1|TV2|Total') {
          final = .x %>% 
            unite(network, TV1:TV2, sep = ' / ') %>% 
            select(gmrank = Rank, gmdate = Date, comp = Competition, teams = Teams, network, viewers = Total)
        }
        return (final)
      }
    )
  ) %>% 
  select(postdate, cleaneddf) %>% 
  unnest(cols = c(cleaneddf))

allgames

allgames %>% write_csv('ratings-uncleaned.csv')
