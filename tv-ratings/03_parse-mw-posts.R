library(rvest)
library(lubridate)
library(tidyverse)

raw.pages = read_csv('most-watched-posts.csv') %>% 
  mutate(rawhtml = map(fname, ~read_html(here::here('most-watched-index', .x))))

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
  select(-link, -fname)

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
        if (.y == 'Rank|Date|Competition|Teams|Network|Total') {
          final = .x %>% 
            select(gmrank = Rank, gmdate = Date, comp = Competition, teams = Teams, network = Network, viewers = Total)
        }
        if (.y == 'Rank|Date|Competition|Teams|TV1|Total') {
          final = .x %>% 
            select(gmrank = Rank, gmdate = Date, comp = Competition, teams = Teams, network = TV1, viewers = Total)
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
  select(postdate, rangestart, rangeend, cleaneddf)

# check
allgames %>% count(parsecols = map_int(cleaneddf, length))

allgames = allgames %>% unnest(cols = c(cleaneddf))

allgames

# "glue" the game date to a date in the post range

datesinranges = allgames %>%
  distinct(rangestart, rangeend) %>% 
  mutate(d = map2(rangestart, rangeend, ~tibble(dateinrange = seq(.x, .y, by = '1 day')))) %>% 
  unnest(cols = c(d))

datesinranges

gmdates = allgames %>% 
  distinct(rangestart, rangeend, gmdate) %>% 
  separate(col = gmdate, into = c('mo', 'da', 'yr'), sep = '/', remove = FALSE) %>% 
  mutate(
    mo = parse_integer(mo),
    da = parse_integer(da),
    yr = parse_integer(str_c('20', yr))
  )

gmdates

gmdates %>% nrow()

datematches = gmdates %>% 
  right_join(datesinranges) %>% 
  mutate(
    momatch = mo == month(dateinrange),
    damatch = da == day(dateinrange)
  )

datematches

# no matches
datematches %>% 
  group_by(rangestart, rangeend, gmdate) %>% 
  summarise(dmatches = sum(damatch)) %>% 
  filter(dmatches != 1)

matcheddates = datematches %>% 
  filter(momatch & damatch) %>% 
  select(rangestart, rangeend, gmdate, gmdatedt = dateinrange) %>% 
  right_join(gmdates) %>% 
  select(-mo, -da, -yr) %>% 
  mutate(
    gmdatedt = case_when(
      is.na(gmdatedt) ~ mdy(gmdate),
      TRUE ~ gmdatedt
    )
  ) %>% 
  mutate(
    gmdatedt = case_when(
      is.na(gmdatedt) & gmdate == '12/16' ~ make_date(2019, 12, 16),
      is.na(gmdatedt) & gmdate == '9/4' ~ make_date(2018, 9, 4),
      TRUE ~ gmdatedt
    )
  )

matcheddates

allgames = allgames %>% 
  left_join(matcheddates) %>% 
  mutate(gmdate = gmdatedt) %>% 
  select(-gmdatedt)

allgames

allgames %>% write_csv('ratings-uncleaned.csv')
