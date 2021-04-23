
# pkgs --------------------------------------------------------------------

library(tidyverse)
library(targets)
library(glue)

# fns ---------------------------------------------------------------------
  
  c(
    "preliminary_scrub",
    "measure_types"
  ) %>% 
    paste0("R/", ., ".R") %>% 
  map(source)
  



list(


# load data ---------------------------------------------------------------

    tar_target(
    adverse_dat,
    read_csv("data-raw/adverse-events.csv")
  ),

  tar_target(
    pain_dat,
    read_csv("data-raw/Pain intensity.csv")
  ),
  
  tar_target(
    outcome_dat,
    list(
      adverse = adverse_dat,
      pain = pain_dat
    )
  ),

  tar_target(
    outcomes_scrubbed,
    preliminary_scrub(outcome_dat %>% pluck(1)),
    pattern = map(outcome_dat),
    iteration = "list"
  ),


# meta data ---------------------------------------------------------------

  tar_target(
    metapar,
    # currently first 8 have the meta info
    outcome_dat %>% 
      pluck("adverse") %>% 
      select(c(1:8))
  ),
  

# wrangling observations --------------------------------------------------


  tar_target(
    long_obs,
    outcomes_scrubbed %>%
      select(-c(2:8)) %>% 
      pivot_longer(
        cols = -study_arm_id,
        names_to = "column_header",
        values_to = "obs",
        values_drop_na = TRUE
      ),
    pattern = map(outcomes_scrubbed),
    iteration = "list"
  ),
  
  tar_target(
    measures,
    measure_types(long_obs),
    pattern = map(long_obs)
  ),

  tar_target(
    wide_measures,
    measures_wide(measures),
    pattern = map(measures)
  ),

  
  
  NULL
)