library(targets)

suppressMessages({
  library(tidyverse)
  
})

list(
  tar_target(check_wd,
                str_detect(getwd(), "simulation")),
  
  # set number of observations in simulated dataset
  tar_target(n_obs,
             3),
  
  # use intuition is the next step
  tar_target(
    set_var,
    tibble(
      outcome = character(),
      condition = character(),
      class = character(),
      type = character()
    )
  ),
  
  tar_target(
    sim_pairwise,
    "not done yet"
  ),
  
  # this is here so I don't have to worry about trailing commas
  NULL
  
  
  
  )
