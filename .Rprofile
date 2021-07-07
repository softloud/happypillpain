source("renv/activate.R")

if (interactive()) {
  installed_pkg <- installed.packages()[,1]
  
  if ("pandan" %in% installed_pkg) {
    require(pandan)
    
  }

  if ("dontpanic" %in% installed_pkg) {
    require(dontpanic)
  }  
  
  if (all(c("devtools", "testthat", "targets") %in% installed_pkg)) {
    require(devtools)
    require(testthat)
    require(targets)
  }
  
  library(tidyverse)
  library(gt)
  conflicted::conflict_prefer("filter", "dplyr")
  
}

options(
  usethis.destdir = "~/Documents/gh-repos/"
)
