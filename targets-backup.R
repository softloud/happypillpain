# load pkgs ---------------------------------------------------------------

library(targets)
suppressMessages(library(here))
options(tidyverse.quiet = TRUE)
library(tidyverse)
suppressMessages(library(lubridate))
suppressMessages(library(janitor))
suppressMessages({
  library(glue)
  library(janitor)
})
library(multinma)
library(metafor)
library(dontpanic)
library(readxl)

# load functions ----------------------------------------------------------

# list.files(here("R"), full.names = TRUE) %>% map(source)
source("R/get_outcome_cols.R")
source("R/preliminary_scrub.R")
source("R/measure_types.R")
source("R/assign_timepoints.R")
source("R/meta_par.R")
source("R/netdat.R")
source("R/pairwise.R")

set_binom_network <- function(obs_binom_network) {
  obs_binom_network %>%
    mutate(r = percent / 100 * n,
           r = as.integer(r)) %>%
    set_agd_arm(
      data = .,
      study = study_identifier,
      trt = intervention_drug,
      trt_ref = "placebo",
      r = r,
      n = n,
      sample_size = n
    )
}


list(
  # raw data ----------------------------------------------------------------
  
  #   tar_target(
  #     raw_dat_path,
  #     # use raw_hpp_dat_path() to get the latest path
  #     # this target is here so it's easier to swap in and out the latest file path
  #     "data-raw/review_91309_extracted_data_csv_20210420002814.csv"
  #   ),
  #   
  #   
  #   # wrangling ---------------------------------------------------------------
  #   tar_target(
  #     colnames_adverse,
  #     read_xlsx("data-raw/Adverse events column names.xlsx") %>%
  #       pluck(1)
  #   ),
  #   
  #   tar_target(
  #     colnames_mood,
  #     read_xlsx("data-raw/Mood and pain column headings.xlsx", sheet = 1) %>% 
  #       pluck(1)
  #   ),
  #   
  #   tar_target(
  #     colnames_pain,
  #     read_xlsx("data-raw/Mood and pain column headings.xlsx", sheet = 2) %>% pluck(1)
  #   ),
  #   
  #   
  #   tar_target(
  #     colnames_outcomes,
  #     list(
  #       adverse = colnames_adverse[-c(1:3)],
  #       mood = colnames_mood[-c(1:3)]#,
  # #      pain = colnames_pain
  #     )
  #   ),
  #   
  #   tar_target(outcomes_of_interest,
  #              colnames_outcomes %>% names()),
  #   
  #   # # stuff for all analyses
  #   tar_target(raw_hpp_dat,
  #              # use raw_hpp_dat_path() to get the latest path
  #              suppressWarnings(
  #                read_csv(file = raw_dat_path,
  #                         col_types = cols(.default = "c"))
  #              ),
  #              format = "rds"),
  
  # get relevant columns for each outcome
  
  
  
  tar_target(
    hpp_dat ,
    # latter part for hashing
    preliminary_scrub(raw_hpp_dat)
  ),
  
  tar_target(
    metapar,
    hpp_dat %>% 
      select(
        c("study_arm_id", colnames_adverse[1:3])
      )
  ),
  
  
  tar_target(
    obs,
    get_outcome_cols(
      outcome = outcomes_of_interest,
      dat = hpp_dat,
      outcome_cols = colnames_outcomes),
    pattern = map(outcomes_of_interest),
    iteration = "list"
  ),
  
  # assign measure types
  tar_target(
    obs_measures,
    measure_types(obs),
    pattern = map(obs),
    iteration = "list"
  ),
  
  # go wide by measures
  tar_target(
    obs_wide_by_measures,
    measures_wide(obs_measures),
    pattern = map(obs_measures),
    iteration = "list"
  ),
  
  # assign timepoints
  tar_target(
    obs_timepoints,
    assign_timepoints(obs_wide_by_measures),
    pattern = map(obs_wide_by_measures),
    iteration = "list"
  ),
  
  # assign meta parameters
  tar_target(
    obs_meta,
    meta_par(obs_timepoints, hpp_dat),
    pattern = map(obs_timepoints),
    iteration = "list"
  ),
  
  # convert measures to numeric
  tar_target(
    obs_numeric,
    measures_numeric(obs_meta),
    pattern = map(obs_meta),
    iteration = "list"
  ),
  
  # now for assumptions, filters atm
  tar_target(
    obs_postint,
    wrangle_netdat(obs_numeric) %>%
      mutate(outcome = outcomes_of_interest),
    pattern = map(obs_numeric, outcomes_of_interest),
    iteration = "list"
  ),
  
  tar_target(
    outcomes,
    tibble(outcome_id = outcomes_of_interest) %>%
      mutate(
        network_id = case_when(
          outcome_id == "substantial pain" ~ "subpain",
          outcome_id == "moderate pain" ~ "modpain",
          outcome_id == "physical function" ~ "physical",
          TRUE ~ outcome_id
        ),
        model_type =  case_when(
          network_id %in% c("subpain", "modpain", "adverse") ~ "binom",
          network_id %in% c("mood", "sleep", "pain", "pgic", "physical", "qol") ~ "smd"
        ),
        outcome_index = row_number()
      )
  ),
  
  tar_target(obs_smd,
             index <-
               obs_postint[c(outcomes %>%
                               dplyr::filter(model_type == "smd") %>%
                               pull(outcome_index))]),
  
  tar_target(obs_binom,
             obs_postint[(outcomes %>%
                            dplyr::filter(model_type == "binom") %>%
                            pull(outcome_index))]),
  
  
  
  # re arm-based model ------------------------------------------------------
  
  # need to try this out on just means
  # what about binomials?
  tar_target(
    network_smd,
    obs_smd %>%
      pluck(1) %>% # why does it need this line? hmmm
      # this is a hack just to get the code to run
      filter(!is.na(n)) %>% 
      set_agd_arm(
        data = .,
        study = study_identifier,
        trt = intervention_drug,
        trt_ref = "placebo",
        y = mean,
        # need to debug this
        se = abs(se),
        sample_size = n
      ),
    pattern = map(obs_smd),
    iteration = "list"
  ),
  
  tar_target(
    model_smd,
    nma(network_smd, trt_effects = "random"),
    pattern = map(network_smd),
    iteration = "list"
  ),
  
  
  # need to try this out on just means
  # what about binomials?
  tar_target(
    network_binom,
    obs_binom %>% pluck(1) %>%
      set_binom_network(),
    pattern = map(obs_binom),
    iteration = "list"
  ),
  
  
  tar_target(
    model_binom,
    nma(network_binom),
    pattern = map(network_binom),
    iteration = "list"
  ),
  
  # NB in the wrangle data function there was a note about single-arm studies, may need to check
  tar_target(models,
             append(model_binom, model_smd) %>%
               map(
                 .f = function(this_model) {
                   this_outcome <-
                     this_model %>%
                     pluck("network", "agd_arm", "outcome") %>%
                     unique()
                   
                   output_model <-
                     append(this_model,
                            list(outcome = this_outcome))
                   class(output_model) <- "stan_nma"
                   output_model
                   
                 }
               )),
  
  
  # re contrast-based models ------------------------------------------------
  
  
  # pairwise ----------------------------------------------------------------
  
  tar_target(pairwise_obs,
             pairwise_wide(obs_postint[[1]], "duloxetine")),
  
  # tar_target(pairwise_smd),
  
  # then do binom
  
  # tar_target(pairwise_models),
  
  
  NULL
)
