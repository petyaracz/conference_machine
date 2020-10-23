##################################################################
##################################################################
# Slot roulette for BCCCD 2021 Conf
# pracz
# v1.0
##################################################################
##################################################################

# Assign presenters from various time zones into fixed conference slots
# Each presenter gets two distinct slots
# Some presenters can't make it to some slots
# Presenters should overlap as little as possible

library(tidyverse)
library(magrittr)

setwd('~/Documents/Móka/bcccd_sampler/')

set.seed(2001)

# --- anonymise abstracts for the github code submission --- #

# abstracts = read_tsv('~/Downloads/BCCCD21 all abstracts - Sheet1.tsv')
# 
# abstracts %<>% 
#   distinct(
#     presenter
#   ) %>% 
#   mutate(
#     presenter_id = paste0('presenter ', 1:n())
#   ) %>% 
#   right_join(abstracts) %>% 
#   select(
#     id, presenter_id, timezone, country
#   )
# 
# write_tsv(abstracts, 'abstracts_anonymised.tsv')

# --- anonymised data --- #

abstracts = read_tsv('abstracts_anonymised.tsv')

# --- add time differences from GMT --- #

abstracts %<>%
  mutate(
    gmt_diff = str_extract(timezone, '(?<=GMT)[+-][0-9][0-9](?=:)') %>% 
      as.double(),
    T1 = ( gmt_diff >= min(gmt_diff) & gmt_diff <= 4 ) | ( gmt_diff >= 12 ),
    T2 = ( gmt_diff >= min(gmt_diff) & gmt_diff <= -7 ) | ( gmt_diff >= 1 ),
    T3 = gmt_diff >= -6 & gmt_diff <= 11,
    possible_time_slots = case_when(
      T1 & T2 & T3 ~ 'T1, T2, T3',
      T1 & T2 ~ 'T1, T2',
      T2 & T3 ~ 'T2, T3',
      T1 & T3 ~ 'T1, T3'
    )
  )

count(abstracts, gmt_diff, T1, T2, T3, possible_time_slots)
count(abstracts, possible_time_slots)

T1_slots = c(1,4,7,10) # 7pm gmt (8pm cet)
T2_slots = c(2,5,8,11) # 7am gmt (8am cet)
T3_slots = c(3,6,9,12) # noon gmt (1pm cet)

t_slots = tibble(
  presentation_session = 1:12
) %>% 
  mutate(
    slot = case_when(
      presentation_session %in% T1_slots ~ 'T1',
      presentation_session %in% T2_slots ~ 'T2',
      presentation_session %in% T3_slots ~ 'T3'
    )
  )

t_times = tibble(
  slot = c('T1','T2','T3'),
  Tokyo = c(04,16,21),
  London = c(19,07,12),
  Budapest = c(20,08,13),
  Boston = c(14,02,07),
  LA = c(11,23,04)
)

times = left_join(t_slots, t_times)

ouch = T
counter = 0
while (ouch){
  
  min_number = 1
  max_number = 100
  i = 1
  reroll = T
  
  while(reroll) {
  
    abstracts %<>%
    rowwise() %>% 
    mutate(
      slot1 = case_when(
        possible_time_slots == 'T1, T2' ~ sample(c(T1_slots,T2_slots), 1),
        possible_time_slots == 'T1, T3' ~ sample(c(T1_slots,T3_slots), 1),
        possible_time_slots == 'T2, T3' ~ sample(c(T2_slots,T3_slots), 1),
        # possible_time_slots == 'T1, T2, T3' ~ sample(1:12, 1) %>% as.double()
        ),
      slot2 = case_when(
        possible_time_slots == 'T1, T2' ~ sample(setdiff(c(T1_slots,T2_slots), slot1), 1),
        possible_time_slots == 'T1, T3' ~ sample(setdiff(c(T1_slots,T3_slots), slot1), 1),
        possible_time_slots == 'T2, T3' ~ sample(setdiff(c(T2_slots,T3_slots), slot1), 1),
        # possible_time_slots == 'T1, T2, T3' ~ sample(setdiff(1:12, slot1), 1) %>% as.double()
      )
    )
    
    slot1s = abstracts %>% filter(!is.na(slot1)) %>% count(slot1)  
    slot2s = abstracts %>% filter(!is.na(slot2)) %>% count(slot2)  
    rr1 = nrow(slot1s) != 12
    rr2 = nrow(slot2s) != 12
    rr3 = min(slot1s$n) < min_number
    rr4 = min(slot2s$n) < min_number
    rr5 = max(slot1s$n) > max_number
    rr6 = max(slot2s$n) > max_number
    
    presenter_overlaps = abstracts %>% 
      count(presenter_id, slot1) %>%
      pull(n) %>% max()
    
    rr7 = presenter_overlaps > 1
    
    reroll = rr1 | rr2 | rr3 | rr4 | rr5 | rr6 | rr7
    print(i)
    i = i + 1
    if (i > 5000 ){ 
      print('first reroll failed')
      break
      }
  }  
  
  count(abstracts, slot1)
  count(abstracts, slot2)
  
  abstracts2 = abstracts
  
  min_number = 16
  max_number = 25
  i = 1
  reroll = T
  
  while(reroll) {
    abstracts2 %<>%
    rowwise() %>% 
    mutate(
      slot1 = case_when(
        possible_time_slots == 'T1, T2, T3' ~ sample(1:12, 1) %>% as.double(),
        T ~ slot1
      ),
      slot2 = case_when(
        possible_time_slots == 'T1, T2, T3' ~ sample(setdiff(1:12, slot1), 1) %>% as.double(),
        T ~ slot2
      )
    )
    slot1s = abstracts2 %>% filter(!is.na(slot1)) %>% count(slot1)  
    slot2s = abstracts2 %>% filter(!is.na(slot2)) %>% count(slot2)  
    rr1 = nrow(slot1s) != 12
    rr2 = nrow(slot2s) != 12
    rr3 = min(slot1s$n) < min_number
    rr4 = min(slot2s$n) < min_number
    rr5 = max(slot1s$n) > max_number
    rr6 = max(slot2s$n) > max_number
    reroll = rr1 | rr2 | rr3 | rr4 | rr5 | rr6
    print(i)
    i = i + 1
    if (i > 5000 ){ 
      print('second reroll failed')
      break
    }
  }
  
  count(abstracts2, slot1)
  count(abstracts2, slot2)
  
  ouches = as.list(NULL)
  for (ii in 1:nrow(abstracts2)){
    
    match = abstracts2[ii,]
    ouch = 0
    
    for (iii in 1:nrow(abstracts2)){
      target = abstracts2[iii,]
      ouch = ifelse(
        ( match$slot1 == target$slot1 ) & ( match$slot2 == target$slot2 ), ouch + 1, ouch 
      )  
  
    }
    ouches[[ii]] = ouch
    # print(ii)  
  }
  
  ouches = unlist(ouches)
  
  ouches
  
  ouch = max(ouches) > 5
  counter = counter + 1
  print(paste0('counter: ', counter))
}