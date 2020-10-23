setwd('~/Documents/Móka/bcccd_sampler/')

library(tidyverse)
library(magrittr)
library(patchwork)

load('abstract_rolls.Rda')

abs = tibble(abstract_rolls)
rm(abstract_rolls)
names(abs) = 'abstract'

singlesOverlap = function(d){  
d %>% 
  count(presenter_id) %>% 
  mutate(single = n == 2) %>% 
  left_join(d, by = 'presenter_id') %>% 
  filter(single) %>% 
  summarise(mean = mean(overlaps))
}  
  
abs %<>%
  mutate(sim = 1:1000)

abs %<>%
  mutate(
    session_count = map(abstract, ~ count(., session, Budapest, LA, Tokyo)),
    overlap_count = map(abstract, ~ summarise(.,
      mean = mean(overlaps),
      sd = sd(overlaps)
    )),
    overlap_count2 = map(abstract, ~ singlesOverlap(.))
  )

abs %>% 
  select(sim, session_count) %>% 
  unnest(cols = c(session_count)) %>% 
  mutate(
    session_Budapest = paste0('S', session, ' ', Budapest, ':00 CET') %>% 
      reorder(-session)
  ) %>% 
  ggplot(aes(x = session_Budapest, y = n)) +
  geom_boxplot() +
  xlab('Sessions (with CET time)') +
  ylab('Number of presentations in session per simulation') +
  ggtitle('Presentations per session across \n conference simulations, n=1000') +
  coord_flip()

ggsave('pres_per_session.pdf', width = 5, height = 6)

p1 = abs %>% 
  select(sim, overlap_count) %>% 
  unnest(cols = c(overlap_count)) %>% 
  ggplot(aes(x = mean)) + 
  geom_histogram() +
  xlim(4,9) +
  ylim(0, 330) +
  xlab('mean number of presentation missed by a participant per simulation') +
  ggtitle('Overlaps across presentations across \n conference simulations, n=1000')

p2 = abs %>% 
  select(sim, overlap_count2) %>% 
  unnest(cols = c(overlap_count2)) %>% 
  ggplot(aes(x = mean)) + 
  geom_histogram() +
  xlab('mean number of presentation missed by\n a participant with one presentation per simulation') +
  ggtitle('Overlaps across presentations for single-presentations \n across conference simulations, n=1000') +
  xlim(4,9) +
  ylim(0,330)

p1 / p2
ggsave('pres_overlaps.pdf', width = 5, height = 6)
