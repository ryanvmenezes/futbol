library(here)
library(rvest)
library(tidyverse)

# url = 'https://worldsoccertalk.com/category/tv-ratings/page/63/'

getorretrieve = function(url, override = FALSE) {
  fname = url %>% 
    str_split('/') %>% 
    `[[`(1) %>% 
    `[`((length(.) - 2):(length(.) - 1)) %>% 
    str_c(collapse = '') %>% 
    str_c('.html')
  
  fpath = here('ratings-post-index', fname)
  
  if (!override & file.exists(fpath)) {
    h = read_html(fpath)
  } else {
    h = read_html(url)
    write_html(h, fpath)
  }
  
  h
}

pages = tibble(indexpage = str_c('https://worldsoccertalk.com/category/tv-ratings/page/', 1:63, '/')) %>% 
  mutate(rawhtml = map(indexpage, getorretrieve))

pages

parsed = pages %>% 
  mutate(
    title = map(
      rawhtml,
      ~.x %>% 
        html_nodes('.latestPost h2 a') %>%
        html_text()
    ),
    link = map(
      rawhtml,
      ~.x %>% 
        html_nodes('.latestPost h2 a') %>%
        html_attr('href')
    ),
    postdate = map(
      rawhtml,
      ~.x %>% 
        html_nodes('.latestPost .post-info .thetime') %>% 
        html_text() %>% 
        str_squish()
    )
  )

parsed

posts = parsed %>% 
  select(-indexpage, -rawhtml) %>% 
  unnest(cols = c(title, link, postdate))

posts

posts %>% write_csv('posts.csv')

