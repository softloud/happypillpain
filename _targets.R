




# pkgs --------------------------------------------------------------------

library(tidyverse)
library(targets)
library(glue)
library(janitor)
library(multinma)
library(tarchetypes)
conflicted::conflict_prefer("filter", "dplyr")

# fns ---------------------------------------------------------------------

c("preliminary_scrub",
  "measure_types",
  "assign_timepoints") %>%
  paste0("R/", ., ".R") %>%
  map(source)



# don't know what this does -----------------------------------------------

tar_option_set(packages = "tidyverse")

list(
  # load data ---------------------------------------------------------------
  
  tar_target(
    adverse_dat,
    read_csv("data-raw/adverse-events.csv") %>%
      preliminary_scrub()
    
  ),
  
  tar_target(
    pain_dat,
    read_csv("data-raw/Pain intensity.csv")  %>%
      preliminary_scrub()
  ),
  
  tar_target(
    mood_dat,
    read_csv("data-raw/Mood.csv")  %>%
      preliminary_scrub()
  ),
  
  tar_target(
    metapar_cols,
    c(
      "study_arm_id",
      "intervention",
      "intervention_drug",
      "intervention_name",
      "intervention_type",
      "study_identifier",
      "study_title"
    )
  ),
  
  tar_target(
    raw_hpp,
    read_csv(
      "/home/cantabile/Documents/gh-repos/happypillpain/data-raw/review_91309_extracted_data_csv_20210420002814.csv",
      col_types = cols(.default = "c")
    )
  ),
  
  tar_target(
    extra_meta_par,
    raw_hpp %>%
      preliminary_scrub() %>% clean_names() %>%
      select(
        study_arm_id,
        study_identifier,
        intervention_class,
        starts_with("main_aim"),
        starts_with("chronic")
      ) %>%
      rename(chronic_condition = chronic_pain_condition_s,
             main_aim = main_aim_pain_mood_quality_of_life_etc) %>%
      # clean chronic condition
      mutate(# make it lower case
        chronic_condition = tolower(chronic_condition))
  ),
  
  tar_target(
    outcome_dat,
    list(
      adverse = adverse_dat,
      pain = pain_dat,
      mood = mood_dat
    ) %>%
      map(
        .f = function(df) {
          df %>%
            select(-metapar_cols[-1])
        }
      )
  ),
  
  tar_target(outcomes_included,
             names(outcome_dat)),
  
  # meta data ---------------------------------------------------------------
  
  tar_target(metapar,
             # currently first 8 have the meta info
             adverse_dat %>%
               select(metapar_cols)),
  
  
  # wrangling observations --------------------------------------------------
  
  tar_target(long_obs,
             outcome_dat %>%
               map(
                 .f = function(df) {
                   df %>%
                     pivot_longer(
                       cols = -study_arm_id,
                       names_to = "column_header",
                       values_to = "obs",
                       values_drop_na = TRUE
                     )
                 }
               )),
  
  tar_target(
    measures,
    long_obs %>% pluck(outcomes_included) %>% measure_types(),
    pattern = map(outcomes_included),
    iteration = "list"
  ),
  
  tar_target(
    measures_obs_wider,
    measures_wide(measures),
    pattern = map(measures),
    iteration = "list"
  ),
  
  tar_target(
    measures_timepoints,
    assign_timepoints(measures_obs_wider),
    pattern = map(measures_obs_wider),
    iteration = "list"
  ),
  
  tar_target(
    measures_convert_numeric,
    measures_numeric(measures_timepoints),
    pattern = map(measures_timepoints),
    iteration = "list"
  ),
  
  tar_target(
    model_key,
    tibble(
      outcome = outcomes_included,
      model = case_when(
        outcome == "pain" ~ "smd",
        outcome == "adverse" ~ "binom",
        outcome == "mood" ~ "smd"
      )
    )
  ),
  
  tar_target(
    obs_by_outcome,
    measures_convert_numeric %>%
      left_join(metapar, by = c("study_arm_id")) %>%
      left_join(extra_meta_par, by = c("study_arm_id", "study_identifier")),
    # map(left_join, y = metapar, by = "study_arm_id")
    pattern = map(measures_convert_numeric),
    iteration = "list"
  ),
  
  # check for duplicate placebos
  tar_target(
    placebo_duplicates,
    obs_by_outcome %>%
      # obs_by_outcome[[1]] %>%
      group_by(study_identifier) %>%
      filter(sum(intervention_drug == "placebo") > 1),
    pattern = map(obs_by_outcome),
    iteration = "list"
  ),
  
  
  # filters, scope ----------------------------------------------------------
  
  
  # timepoint
  
  tar_target(
    post_int,
    obs_by_outcome %>%
      filter(timepoint == "post_int"),
    pattern = map(obs_by_outcome),
    iteration = "list"
  ),
  
  tar_target(
    not_post_int,
    obs_by_outcome %>%
      filter(timepoint != "post_int"),
    pattern = map(obs_by_outcome),
    iteration = "list"
  ),
  
  
  
  # split to smd & binom ----------------------------------------------------
  
  tar_target(obs_smd,
             post_int[model_key$model == "smd"]  %>%
               map(
                 .f = function(df) {
                   df %>%
                     mutate(sd = if_else(is.na(sd),
                                         sqrt(n) * se,
                                         sd),
                            se = if_else(is.na(se),
                                         sd / sqrt(n),
                                         se)) %>%
                     # this needs to be investigated further
                     filter(!is.na(n) & !is.na(mean) & !is.na(se))
                 }
               )),
  # sort out single-arm studies
  
  tar_target(obs_binom,
             post_int[model_key$model == "binom"] %>% unname()),
  
  
  tar_target(
    binom_adverse_w_single_arm,
    
    # 'cause there's only one right now
    obs_binom[[1]] %>%
      rename(r = n) %>%
      mutate(n = (100 * r) / percent,
             n = round(n)) %>%
      filter(!is.na(r) & !is.na(n))
  ),
  
  tar_target(
    adverse_single_arm,
    binom_adverse_w_single_arm %>%
      group_by(study_identifier) %>%
      filter(length(intervention) == 1)
    
  ),
  
  tar_target(
    adverse_obs,
    binom_adverse_w_single_arm %>%
      group_by(study_identifier) %>%
      filter(length(intervention) > 1)
    
  ),
  
  tar_target(
    outcome_obs,
    list(
      adverse = adverse_obs,
      pain = obs_smd[[1]],
      mood = obs_smd[[2]]
    )
  ),
  
  # nma no moderators -------------------------------------------------------
  
  tar_target(
    nomod_arm_smd,
    map2(
      obs_smd,
      model_key %>% filter(outcome == "smd") %>% pull(outcome),
      .f = function(df, outcome) {
        set_agd_arm(
          data = df,
          y = mean,
          # todo standard errors are not all positive
          se = abs(se),
          sample_size = n,
          trt_ref = "placebo",
          trt = intervention_drug,
          study = study_identifier
        ) %>%
          nma(trt_effects = "random") %>%
          append(list(hpp_outcome = outcome))
        
      }
    )
  ),
  
  tar_target(
    nomod_arm_adverse,
    set_agd_arm(
      data = adverse_obs,
      r = r,
      n = n,
      trt_ref = "placebo",
      trt = intervention_drug,
      study = study_identifier
    ) %>%
      nma(trt_effects = "random") %>%
      append(list(outcome_hpp = "adverse"))
    
  ),
  
  
  
  # type --------------------------------------------------------------------
  
  tar_target(m_nomod_arm,
             
             nomod_arm_smd %>%
               append(list(adverse = nomod_arm_adverse))),
  
  # sticking this here to test ----------------------------------------------
  
  
  # by class of drug
  tar_target(
    drug_class_outcome,
    # post_int %>%
    #   map(filter, intervention_type != "Placebo") %>%
    #   map(group_by, intervention_type) %>%
    #   map(summarise, studies = n_distinct(study_identifier)) %>%
    #   map(arrange, desc(studies)) %>%
    #   map(filter, )
    
    post_int %>%
      filter(
        intervention_type != "Placebo",
        intervention_type != "Active placebo"
      ) %>%
      group_by(intervention_class) %>%
      summarise(studies = n_distinct(study_identifier)) %>%
      arrange(desc(studies)) %>%
      filter(studies > 3,!is.na(intervention_class)) %>%
      mutate(outcome = outcomes_included),
    pattern = map(post_int, outcomes_included)
  ),
  
  tar_target(
    class_grouping,
    drug_class_outcome %>%
      left_join(model_key, by = "outcome") %>% 
      group_by(intervention_class, outcome) %>%
      tar_group(),
    iteration = "group"
  ),
  
  tar_target(
    class_grouping_list,
    class_grouping,
    iteration = "list"
  ),
  
  tar_target(
    nma_class,
    outcome_obs %>% 
      pluck(class_grouping %>% pull(outcome)) %>%
      filter(intervention_class == class_grouping$intervention_class) # %>%
      # if (class_grouping$model == "binom") {
      #   "test"
      #   # set_agd_arm(
      #   #   study = study_identifier,
      #   #   trt = intervention,
      #   #   r = r,
      #   #   n = n,
      #   #   trt_ref = "placebo"
      #   # )
      # } else if ((class_grouping %>% pull(model)) == "smd") {
      #   "haha haven't coded this yet"
      # 
      # } else {stop("hmm this appears to be an error")}
      ,
    pattern = map(class_grouping)
  ),
  
  # tar_target(
  #   chronic_condition_outcome,
  #   post_int %>%
  #     filter(
  #       intervention_type != "Placebo",
  #       intervention_type != "Active placebo"
  #     ) %>%
  #     group_by(chronic_condition) %>%
  #     summarise(studies = n_distinct(study_identifier)) %>%
  #     arrange(desc(studies)) %>%
  #     filter(studies > 3) %>%
  #     mutate(outcome = outcomes_included),
  #   pattern = map(post_int, outcomes_included)
  # ),
  
  
  # pairwise ----------------------------------------------------------------
  
  
  
  
  NULL
)
