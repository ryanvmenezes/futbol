library(here)
library(rvest)
library(tidyverse)

post.index = read_csv('posts.csv')

post.index

mw.post.index = post.index %>% 
  filter(str_detect(title, 'Most-watched')) %>% 
  arrange(desc(link))

mw.post.index

createfilename = function(url) {
  url %>% 
    str_split('/') %>% 
    `[[`(1) %>% 
    `[`(length(.) - 1) %>%
    str_c('.html')
}

getorretrieve = function(url, fname, override = FALSE) {
  fpath = here('most-watched-index', fname)
  
  if (!override & file.exists(fpath)) {
    h = read_html(fpath)
  } else {
    h = read_html(url)
    write_html(h, fpath)
  }
  
  h
}

raw.pages = mw.post.index %>% 
  mutate(
    fname = map_chr(link, createfilename),
    rawhtml = map2(link, fname, getorretrieve)
  )

raw.pages

raw.pages %>%
  select(-rawhtml) %>% 
  write_csv('most-watched-posts.csv')
