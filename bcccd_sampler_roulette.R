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
library(furrr)

setwd('~/Documents/Móka/bcccd_sampler/')

set.seed(2001)

# --- anonymise abstracts for the github code submission --- #
# abstracts = read_tsv('~/Downloads/BCCCD21 all abstracts - Sheet1.tsv')
# 
# # need to fix presenter/submitter names
# 
# abstracts %<>%
#   filter(
#     decision == 'accept'
#   ) 
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
# abstracts %<>%
#   mutate(
#     gmt_diff = str_extract(timezone, '(?<=GMT)[+-][0-9][0-9](?=:)') %>%
#       as.double(),
#     T1 = ( gmt_diff >= min(gmt_diff) & gmt_diff <= 4 ) | ( gmt_diff >= 12 ),
#     T2 = ( gmt_diff >= min(gmt_diff) & gmt_diff <= -7 ) | ( gmt_diff >= 1 ),
#     T3 = gmt_diff >= -6 & gmt_diff <= 11,
#     possible_time_slots = case_when(
#       T1 & T2 & T3 ~ 'T1, T2, T3',
#       T1 & T2 ~ 'T1, T2',
#       T2 & T3 ~ 'T2, T3',
#       T1 & T3 ~ 'T1, T3'
#     )
#   )
# 
# write_tsv(abstracts, 'abstracts_anonymised.tsv')

# --- anonymised data --- #

abstracts = read_tsv('abstracts_anonymised.tsv')

# --- time slots --- #

T1_slots = c(1,4,7,10) # 7pm gmt (8pm cet)
T2_slots = c(2,5,8,11) # 7am gmt (8am cet)
T3_slots = c(3,6,9,12) # noon gmt (1pm cet)

# --- functions --- #

# take three slots, return time table
buildTimeSlots = function(T1_slots, T2_slots, T3_slots){
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
  
  left_join(t_slots, t_times, by = 'slot')
}

# take abstracts, roll for time slots, return abstracts w/ slots in long format
rerollAbstracts = function(abstracts){
  
  # randomise abstracts
  abstracts = sample_n(abstracts, nrow(abstracts))
  
  # we need to assign slots to presenters, not presentations, to check for overlaps.
  presenters = abstracts %>% 
    count(presenter_id, possible_time_slots) %>%
    rowwise() %>% 
    mutate(
      session = list(case_when(
        # for t2, t3 give n slots for each, where n is number of presentations by presenter
        possible_time_slots == 'T2, T3' ~ c(
          sample(T2_slots, n), 
          sample(T3_slots, n)
        ),
        # same here w/ t1 t2
        possible_time_slots == 'T1, T2' ~ c(
          sample(T1_slots, n), 
          sample(T2_slots, n)
        ),
        # same here w/ t1 t3
        possible_time_slots == 'T1, T3' ~ c(
          sample(T1_slots, n), 
          sample(T3_slots, n)
        ),
        # for the "whatever" group, one slot has to be t2 so all can see, other one can be t1 or t3
        possible_time_slots == 'T1, T2, T3' ~ c(
          sample(T2_slots, n), 
          sample(c(T1_slots,T3_slots), n)
        )
      )
      )
    )
  
  # unnest this so we get one session per line
  presenter_slots = presenters %>% 
    unnest(cols = c(session)) %>% 
    arrange(presenter_id)
  
  # take abstracts, make space for slot1
  abstracts_presenters = abstracts %>% 
    arrange(presenter_id,id) %>% 
    mutate(slot = 'slot1')
  
  # take abstracts, make space for slot2, bind rows so we have abstract list with two rows (two slots) per presentation
  abstracts_presenters  = abstracts %>% 
    arrange(presenter_id,id) %>% 
    mutate(slot = 'slot2') %>% 
    bind_rows(abstracts_presenters) %>% 
    arrange(presenter_id)
  
  # we row_bind abstracts and presenter slots.
  abstracts_presenters$session = presenter_slots$session
  
  # this is the new abstract list with time slots added.
  abstracts2 = abstracts_presenters
  
  # we need this in wide form to count overlaps
  abstracts2w = abstracts2 %>% 
    select(id, presenter_id, slot, session) %>% 
    pivot_wider(names_from = slot, values_from = session)
  
  # how many overlaps? how many presentations must an individual presenter skip?
  ouches = as.list(NULL) # list for overlap count per presenter
  for (ii in 1:nrow(presenters)){ # we loop thru presenters
    
    match = presenters[ii,]$session %>% unlist # take their slots
    ouch = 0 # set ouch to 0
    
    for (iii in 1:nrow(abstracts2w)){ # loop thru ABSTRACTS in wide form (indiv presentations)
      target = c(abstracts2w[iii,]$slot1, abstracts2w[iii,]$slot2) # take their slots
      ouch = ifelse( # if BOTH slots overlap w/ ii, that means the iith presenter can't see either of these slots, and so will miss this presentation. we add one to ouch. otherwise we keep ouch. 
        length(intersect(match, target)) == 2, ouch + 1, ouch
      )
    }
    
    ouches[[ii]] = ouch # we store the ouch value for this presenter
  }
  
  # we looped thru presenters in order, so we can row bind the ouch values to the presenter df directly
  presenters$overlaps = unlist(ouches)
  # presenters overlap with themselves, but we don't care about that
  presenters$overlaps = presenters$overlaps - presenters$n
  
  # we add presenter overlap info to abstracts
  abstracts2 = presenters %>% 
    select(presenter_id,overlaps) %>% 
    left_join(abstracts2, by = "presenter_id")
  
  # we add times info to abstracts
  abstracts2 = times %>% 
    rename(
      T_slot = slot,
      session = presentation_session
    ) %>% 
    right_join(abstracts2, by = "session")
  
  # we keep relevant cols.
  abstracts2 %>% 
    select(
      id, 
      presenter_id, 
      country, 
      timezone, 
      gmt_diff, 
      possible_time_slots,
      overlaps,
      slot,
      session,
      T_slot,
      Tokyo,
      London,
      Budapest,
      Boston,
      LA
    )
  
}


# --- set up time slot info --- #

times = buildTimeSlots(T1_slots, T2_slots, T3_slots)

# -- distribute speakers across slots --- #

tictoc::tic('one roll')
abstracts2 = rerollAbstracts(abstracts)
tictoc::toc()




