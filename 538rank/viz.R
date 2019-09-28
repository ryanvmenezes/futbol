library(tidyverse)

data = read_csv('https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv')

lgsummary = data %>% 
  group_by(league) %>% 
  summarise(numteams = n(),
            topspi = max(spi),
            bottomspi = min(spi),
            medianspi = median(spi),
            meanspi = mean(spi),
            meanoff = mean(off),
            meandef = mean(def),
            sdspi = sd(spi))

lgsummary %>%  
  filter(!str_detect(str_to_lower(league), 'clausura|uefa')) %>% 
  mutate(lgrankname = str_c(rank(-topspi, ties.method = 'first'), '. ', league)) %>% 
  left_join(data) %>% 
  ggplot(aes(fct_reorder(lgrankname, topspi), spi)) +
  geom_boxplot() +
  geom_point(alpha = 0.2) +
  coord_flip() +
  ylab('ESPN Soccer Power Index rating of teams') +
  xlab('') +
  ggtitle('All leagues by best team')
