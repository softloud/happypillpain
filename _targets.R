# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

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

# Set target-specific options such as packages.
# tar_option_set(packages = "dplyr") e.g.

# notes on this analysis
# assessing the efficacy of antidepressants by type, class and dose in
# improving pain, mood, patient global impression of change, physical
# functioning, sleep quality and quality of life;
# • assessing the number of adverse events of antidepressants by type, class
# and dose;
# • ranking antidepressants in the efficacy of treating pain, negative affect,
# and adverse events.

# For pain and mood, where applicable we will also dichotomise outcomes into pain
# relief or improvement of 50% or greater, in line with the Initiative in Methods,
# Measurement and Pain Assessment in Clinical Trials (IMMPACT) guidance, to
# indicate substantial improvement (Dworkin 2008).

# timepoints
#
#
# We will compare antidepressants to the comparators
# - immediately post-treatment,
# - short term follow up (<=12 weeks)
# - long-term follow up (>12 weeks) periods.
#
# Where studies include multiple follow
# up timepoints, we will take the most recent timepoint within each period.
# If multiple measures are used, then we will extract from the most valid,
# reliable and widely used measure in the field.


# update data -------------------------------------------------------------

# todo: convert update_hpp_dat() keywords stuff to create keywords

# load functions ----------------------------------------------------------

list.files(here("R"), full.names = TRUE) %>% map(source)


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
  # wrangling ---------------------------------------------------------------
  tar_target(
    colnames_adverse,
    read_csv("data-raw/Adverse events column names.xlsx")
  ),
  
  tar_target(
    colnames_moodpain,
    read_csv("data-raw/Mood and pain column headings.xlsx")
  ),
  
  
  tar_target(
    raw_dat_path,
    # use raw_hpp_dat_path() to get the latest path
    # this target is here so it's easier to swap in and out the latest file path
    "data-raw/review_91309_extracted_data_csv_20210408052653.csv"
  ),
  tar_target(data_update_date,
             raw_hpp_dat_date(raw_dat_path)),
  # stuff for all analyses
  tar_target(raw_hpp_dat,
             # use raw_hpp_dat_path() to get the latest path
             suppressWarnings(
               read_csv(file = raw_dat_path,
                        col_types = cols(.default = "c"))
             ),
             format = "rds"),
  tar_target(keywords,
             read_csv("data-raw/keywords.csv")),
  tar_target(
    hpp_dat ,
    # latter part for hashing
    preliminary_scrub(raw_hpp_dat) %>% dontpanic::lotr_study_hash(
      study_col = study_identifier,
      trt_col = intervention_drug,
      sep = " "
    )
  ),
  
  tar_target(all_outcomes,
             keywords %>%
               pull(outcome) %>%
               unique()),
  
  tar_target(outcomes_excluded_from_pipeline,
             all_outcomes[-c(1, 6, 7)]),
  
  tar_target(outcomes_of_interest,
             all_outcomes[c(1, 6, 7)]),
  
  # get measure observations
  tar_target(
    obs,
    # added keywords
    extract_obs(outcomes_of_interest, hpp_dat, keywords),
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
               obs_postint[c(outcomes %>% dplyr::filter(model_type == "smd") %>% pull(outcome_index))]),
  
  tar_target(obs_binom,
             obs_postint[(outcomes %>% dplyr::filter(model_type == "binom") %>% pull(outcome_index))]),
  
  
  
  # re arm-based model ------------------------------------------------------
  
  # need to try this out on just means
  # what about binomials?
  tar_target(
    network_smd,
    obs_smd %>%
      pluck(1) %>% # why does it need this line? hmmm
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
  
  tar_target(pairwise_smd,
             obs_smd,
             # %>%
             # go wide first
             # escalc() %>%
             # rma(),
             pattern = map(obs_smd)),
  
  # tar_target(pairwise_smd),
  
  # then do binom
  
  # tar_target(pairwise_models),
  
  
  # update site -------------------------------------------------------------
  
  # don't know why this doesn't work
  
  # tar_target(update_outcomes,
  #            {
  #              models %>%
  #                map_chr("outcome") %>%
  #                map(
  #                  .f = function(outcome) {
  #                    file_name_outcome <- str_replace_all(outcome, " ", "-")
  #                    rmarkdown::render(
  #                      input = "template-outcome-analysis-generator.Rmd",
  #                      output_file = glue("docs/outcome-analysis-{file_name_outcome}.html"),
  #                      params = list(outcome = outcome)
  #                    )
  #                  }
  #
  #                )
  #
  #            }),
  # tar_target(update_site,
  #
  #            rmarkdown::render_site()
  #
  #            )
  # ,
  #
  NULL
)
