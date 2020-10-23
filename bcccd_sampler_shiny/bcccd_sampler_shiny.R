library(tidyverse)
library(magrittr)
library(shiny)
library(DT)

set.seed(2021)

# --- fun --- #

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

rerollAbstracts2 = function(abstracts_in,times){
  
  # randomise abstracts
  abstracts = sample_n(abstracts_in, nrow(abstracts_in))
  
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
  
  # we add times info to abstracts
  abstracts2 = times %>% 
    rename(
      T_slot = slot,
      session = presentation_session
    ) %>% 
    right_join(abstracts2, by = "session")
  
  # we keep relevant cols.
  abstracts_out = abstracts2 %>% 
    rename(
      presentation_id = id
    ) %>% 
    select(
      presentation_id, 
      presenter_id, 
      country, 
      timezone, 
      gmt_diff, 
      possible_time_slots,
      # overlaps,
      slot,
      session,
      T_slot,
      Tokyo,
      London,
      Budapest,
      Boston,
      LA
    )
  
  return(abstracts_out)
}

# --- dat --- #

abstracts_in = read_tsv('abstracts_anonymised.tsv')

T1_slots = c(1,4,7,10) # 7pm gmt (8pm cet)
T2_slots = c(2,5,8,11) # 7am gmt (8am cet)
T3_slots = c(3,6,9,12) # noon gmt (1pm cet)

# --- run --- #

times = buildTimeSlots(T1_slots, T2_slots, T3_slots)

# --- shiny --- #

ui <- fluidPage(
  
  # Application title
  titlePanel("BCCCD21 schedule simulator"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("go","Simulate conference"),
      helpText("Pressing the button generates a conference schedule. The schedule will show each presentation slot in one row, with its assigned times. This means that each presentation is two rows, one for slot one, one for slot two. We accepted 221 presentations, which translates to 442 rows in the table. Country, timezone, difference from GMT, and possible time slots are relevant for the presenter. Slot, session, T slot, and local times are relevant for the presentation slot. Each presentation can happen in one of 12 presentation sessions. Local times indicate when these sessions take place. You can sort the table using the arrows after the column names.")
    ),
    
  
  mainPanel(
      DT::dataTableOutput("out")
    )
  )
)

server <- function(input, output) {
  
  restab <- eventReactive(input$go,{
    DT::datatable(rerollAbstracts2(abstracts_in,times))
  })
  output$out <- DT::renderDataTable(restab())
}

shinyApp(ui = ui, server = server)