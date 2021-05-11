library(targets)

suppressMessages({
  library(tidyverse)
  
})

list(tar_target(check_wd,
                str_detect(getwd(), "simulation")))
