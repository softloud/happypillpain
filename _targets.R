# pkgs --------------------------------------------------------------------
library(targets)
library(tidyverse)
library(tarchetypes)
library(janitor)
library(glue)


# functions ---------------------------------------------------------------
c("study_id",
  "clean_colvals") %>%
  paste0("R/", ., ".R") %>%
  map(source)



list(
  # get raw data ------------------------------------------------------------
  
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
  
  tar_target(
    raw_outcome_dat,
    list(
      adverse = raw_adverse %>% mutate(outcome = "adverse"),
      pain = raw_pain %>% mutate(outcome = "pain"),
      mood = raw_mood %>% mutate(outcome = "mood")
    )
  ),
  
  tar_target(
    scales,
    read_csv("data-raw/scales.csv") %>%
      clean_names() %>%
      mutate(across(everything(), tolower)) %>%
      # this is just for now
      mutate(
        outcome = str_extract(outcome, "\\w+"),
        aka = str_replace_all(aka, "\\s*;\\s*", "|")
      ) %>%
      unite(
        col = "pattern",
        scale,
        aka,
        sep = "|",
        remove = FALSE,
        na.rm = TRUE
      )
    %>% select(-aka) %>%
      mutate(
        scale = str_replace_all(scale, " - ", "_"),
        scale = str_replace_all(scale, "-", "_"),
        scale = str_replace_all(scale, " ", "_")
      )
  ),
  
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
  
  tar_target(outcomes,
             model_key %>% pull(outcome)),
  
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
        arm = intervention,
        class = intervention_class,
        type = intervention_type,
        name = intervention_name,
        chronic_condition,
        main_aim,
        everything()
      ) %>%
      mutate(across(everything(), tolower))
  ),
  
  tar_target(metapar,
             metapar_consult %>%
               mutate(
                 name = if_else(
                   is.na(name) &
                     type == "antidepressant" &
                     str_detect(study, "tasmuth"),
                   "venlafaxine",
                   name
                 )
               )),
  
  
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
    wo_long,
    wo_study %>%
      # wo_study[[3]] %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = -c(outcome, study_arm),
        names_to = "covidence_colname",
        values_to = "covidence_value",
        values_drop_na = TRUE
      ) %>%
      mutate(
        measure_matches = map(covidence_colname, str_match, "(.+)_([a-z]+)[_\\d]*$"),
        measure_type = map_chr(measure_matches, 3),
        measure_desc = map_chr(measure_matches, 2)
      ) %>%
      select(-measure_matches, -covidence_colname)
    ,
    pattern = map(wo_study),
    iteration = "list"
  ),
  
  tar_target(
    wo_wide,
    wo_long %>%
      pivot_wider(
        id_cols = c(outcome, study_arm, measure_desc),
        names_from = measure_type,
        values_from = covidence_value
      ) %>%
      mutate(across(any_of(
        c("mean", "sd", "se", "percent")
      ), as.numeric)) %>%
      mutate(n = as.integer(n)),
    pattern = map(wo_long),
    iteration = "list"
  ),
  
  tar_target(
    wo_time,
    wo_wide %>%
      dplyr::mutate(
        timepoint = dplyr::case_when(
          stringr::str_detect(measure_desc, "post_intervention|endpoint|ednpoint") ~ "post_int",
          str_detect(measure_desc, "mid_intervention|within_trial") ~ "mid_int",
          str_detect(measure_desc, "baseline") ~ "baseline",
          TRUE ~ "unmatched"
        )
      ),
    pattern = map(wo_wide),
    iteration = "list"
  ),
  
  tar_target(
    wo_time_unmatched,
    wo_time %>%
      filter(timepoint == "unmatched") %>%
      select(outcome, study_arm, measure_desc, timepoint) %>%
      distinct(),
    pattern = map(wo_time)
  ),
  
  tar_target(
    wo_scale,
    wo_time %>%
      mutate(scale_primary_tag = map2(
        measure_desc,
        outcome,
        .f = function(mdesc, outc) {
          primary_tags <-
            scales %>%
            filter(outcome == outc)
        }
      )),
    pattern = map(wo_time)
      
      ),
    
    tar_target(wo,
               wo_time,
               iteration = "list"),
    
    # filters -----------------------------------------------------------------
    
    tar_target(
      obs_post_int,
      wo %>%
        filter(timepoint == "post_int"),
      pattern = map(wo),
      iteration = "list"
    ),
    # models ------------------------------------------------------------------
    
    # this is just here so I don't have to worry about final commas
    # when testing
    NULL
  )
  