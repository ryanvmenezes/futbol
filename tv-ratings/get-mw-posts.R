library(here)
library(rvest)
library(lubridate)
library(tidyverse)

post.index = read_csv('posts.csv')

post.index

mw.post.index = post.index %>% 
  filter(str_detect(title, 'Most-watched')) %>% 
  arrange(desc(link))

mw.post.index

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

raw.pages = mw.post.index %>% 
  mutate(rawhtml = map(link, getorretrieve))

raw.pages

parsed.pages = raw.pages %>% 
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

parsed.pages

complete.mw.pages = parsed.pages %>% 
  mutate(postdate = mdy(postdate)) %>% 
  filter(postdate >= ymd('2016-11-10')) %>% 
  filter(str_detect(cnames, 'Compe'))

complete.mw.pages

date.range.parsing = complete.mw.pages %>% 
  transmute(
    title,
    daterange = str_replace(title, 'Most-watched soccer games on US TV for (week of )?', ''),
    daterange = case_when(
      daterange == 'February 26 to March 4, 2019' ~ 'February 26-March 4, 2019',
      daterange == 'December 26-January 7' ~ 'December 26-January 7, 2018',
      daterange == 'December 6 to 12, 2016' ~ 'December 6-12, 2016',
      TRUE ~ daterange
    )
  ) %>% 
  extract(
    col = daterange,
    into = c('startmonth', 'startdate', 'endmonth', 'enddate', 'startyear'),
    regex = '^(\\w*)\\s(\\d+)-([a-zA-Z]*\\s)?(\\d+),\\s(\\d+)',
    remove = FALSE
  ) %>% 
  mutate(
    endyear = startyear, 
    startyear = case_when(
      daterange == 'December 26-January 7, 2018' ~ '2017',
      daterange == 'December 27-January 1, 2017' ~ '2016',
      TRUE ~ endyear
    ),
    endmonth = case_when(
      is.na(endmonth) ~ startmonth,
      TRUE ~ endmonth
    ),
    endmonth = str_trim(endmonth)
  ) %>% 
  unite(
    col = 'startmdy',
    startmonth, startdate, startyear,
    sep = ' ',
    remove = FALSE
  ) %>% 
  unite(
    col = 'endmdy',
    endmonth, enddate, endyear,
    sep = ' ',
    remove = FALSE
  ) %>% 
  mutate(
    rangestart = mdy(startmdy),
    rangeend = mdy(endmdy)
  )

date.range.parsing

complete.mw.pages

complete.mw.pages %>% count(cnames) %>% arrange(-n)

allgames = complete.mw.pages %>% 
  left_join(
    date.range.parsing %>% 
      select(title, rangestart, rangeend)
  ) %>% 
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
  select(postdate, rangestart, rangeend, cleaneddf) %>% 
  unnest(cols = c(cleaneddf))

allgames

allgames %>% write_csv('ratings-uncleaned.csv')

# allgames %>% distinct(gmdate) %>% View()
