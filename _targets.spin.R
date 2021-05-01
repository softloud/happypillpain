# pkgs --------------------------------------------------------------------
library(targets)
library(tidyverse)
library(tarchetypes)
library(janitor)
library(glue)


# functions ---------------------------------------------------------------
c("study_id") %>%
  paste0("R/", ., ".R") %>%
  map(source)



list(
  # get data ----------------------------------------------------------------
  
  tar_target(
    raw_covidence_export,
    read_csv(
      "/home/cantabile/Documents/gh-repos/happypillpain/data-raw/review_91309_extracted_data_csv_20210420002814.csv",
      col_types = cols(.default = "c")
    )
  ),
  
  tar_target(raw_adverse,
             read_csv("data-raw/adverse-events.csv")),
  
  tar_target(raw_pain,
             read_csv("data-raw/Pain intensity.csv")),
  
  tar_target(raw_mood,
             read_csv("data-raw/Mood.csv")),
  
  tar_target(raw_outcome_dat,
             list(adverse = raw_adverse,
                  pain = raw_pain,
                  mood = raw_mood)),
  
  
  # metaparameters ----------------------------------------------------------
  
  tar_target(
    model_key,
    # make sure this matches raw_outcome_dat
    tibble(outcome = c("adverse",
                       "pain",
                       "mood")) %>%
      mutate(
        response_measure = case_when(
          outcome == "adverse" ~ "lor",
          outcome == "pain" ~ "smd",
          outcome == "mood" ~ "smd"
        )
      )
  ),
  
  tar_target(
    outcomes,
    model_key %>% pull(outcome)
  ),
  
  tar_target(
    metapar_consult,
    raw_covidence_export %>%
      clean_names() %>%
      select(
        study_identifier,
        study_title = comments,
        intervention,
        intervention_type,
        intervention_class,
        intervention_name,
        chronic_condition = chronic_pain_condition_s,
        main_aim = main_aim_pain_mood_quality_of_life_etc
      ) %>%
      study_id() %>%
      select(
        study,
        intervention,
        intervention_class,
        chronic_condition,
        main_aim,
        everything()
      ) %>% 
      mutate(across(everything(), tolower))
  ),
  
  tar_target(
    metapar,
    metapar_consult %>% 
      mutate(
        intervention_name = if_else(
          is.na(intervention_name) & intervention_type == "antidepressant" & str_detect(study, "tasmuth"),
          "venlafaxine",
          intervention_name
        )
      )
  ),
  
  
  # wrangle outcomes --------------------------------------------------------
  
  tar_target(
    wo_study,
    raw_outcome_dat %>%
      pluck(outcomes) %>%
      clean_names() %>% 
      rename(study_title = comments) %>%
      select(-intervention_type,-intervention_name) %>%
      study_id() %>% 
      select(-study_identifier, -study_title, -study, -intervention) %>% 
      select(study_arm, everything())
    ,
    pattern = map(outcomes),
    iteration = "list"
  ),
  
  tar_target(
    wo_measures,
    wo_study 
  ),
  
  
  
  # models ------------------------------------------------------------------
  
  
  
  
  # this is just here so I don't have to worry about final commas
  # when testing
  NULL
)
