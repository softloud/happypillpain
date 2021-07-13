
# pkgs and setup ----------------------------------------------------------

library(googlesheets4)
library(glue)

atm <- lubridate::now() %>% 
  str_replace_all("\\s", "_")

# variables ---------------------------------------------------------------

system("rm data/variables*.csv")

variables <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1nCusPKeikU8oU-8yp1T503AayTDA1mssAGKvi-96lsY/edit#gid=1391233626")

variables_file <- glue("data/variables-{atm}.csv")

write_csv(variables, variables_file)

# system(glue("sed 's/data\\/variables-*.csv/{variables_file}/g' _targets.R"))

# scales ------------------------------------------------------------------

system("rm data/scales*.csv")

scales <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1Q8QuZ9avto7TlBvHMpcPyX0NFI3R9nLevVjqBXDlHGg/edit#gid=0",
             sheet = "All outcomes")

write_rds(scales, glue("data/scales-{atm}.rds"))

# conditions --------------------------------------------------------------

system("rm data/conditions*.csv")

conditions <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1D4ccwbILjqxk3QPZdq2p4HdvkKBkfJVETXCQ2XhuNe4/edit#gid=269404221",
             sheet = "chronic_condition")

write_csv(conditions, glue("data/conditions-{atm}.csv"))