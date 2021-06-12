# pkgs --------------------------------------------------------------------
suppressMessages({
  library(targets)
  library(tidyverse)
  library(tarchetypes)
  library(janitor)
  library(glue)
  # library(dontpanic)
  library(gt)
  library(assertthat)
  library(rmarkdown)
  
  conflicted::conflict_prefer("filter", "dplyr")
  
})

installed_pkg <- installed.packages()[, 1]

if ("dontpanic" %in% installed_pkg) {
  require(dontpanic)
}




# functions ---------------------------------------------------------------
c("study_id",
  "clean_colvals",
  "scale_match",
  "lotr_study_hash",
  "hpp_themes") %>%
  paste0("R/", ., ".R") %>%
  map(source)



# shorthand ---------------------------------------------------------------

# run specific parts of the pipeline: starts_with *_

# starts_with labels

# r := raw
# w := wrangling
# m := model
# c := check, for exporting checks and asserts
# e := exploratory data analysis table or vis

# secondary signifiers ^_*_

# h := prepared by hollie


list(
  
  
  NULL
)
