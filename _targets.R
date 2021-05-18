# pkgs --------------------------------------------------------------------
suppressMessages({
  library(targets)
  library(tidyverse)
  library(tarchetypes)
  library(janitor)
  library(glue)
  library(dontpanic)
  library(gt)
  library(assertthat)
  
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



list(
  # raw data observations ------------------------------------------------------------
  
  
  tar_target(raw_adverse,
             read_csv("data/adverse-events.csv")),
  
  tar_target(raw_pain,
             read_csv("data/Pain intensity.csv")),
  
  tar_target(raw_mood,
             read_csv("data/Mood.csv")),
  
  tar_target(
    raw_outcome_dat,
    list(
      adverse = raw_adverse %>% mutate(outcome = "adverse"),
      pain = raw_pain %>% mutate(outcome = "pain"),
      mood_depression = raw_mood %>% mutate(outcome = "mood_depression")
    )
  ),
  
  
  # raw labels --------------------------------------------------------------
  
  
  
  tar_target(scales_raw,
             read_csv("data/scales.csv") %>% clean_names()),
  
  
  
  # raw variables, moderators, subgroups ----------------------------------------
  
  tar_target(
    raw_covidence_export,
    read_csv(
      "data/review_91309_extracted_data_csv_20210518145407.csv",
      col_types = cols(.default = "c")
    )
  ),
  
  
  tar_target(classifiers,
             read_csv("data/classifiers.csv")),
  
  tar_target(variables,
             read_csv("data/variables.csv")),
  
  
  # metaparameters ----------------------------------------------------------
  
  # assign measure of comparison for each outcome
  tar_target(
    model_key,
    # make sure this matches raw_outcome_dat
    tibble(outcome = c("adverse",
                       "pain",
                       "mood_depression")) %>%
      mutate(
        response_measure = case_when(
          outcome == "adverse" ~ "lor",
          # log-odds ratio
          outcome == "pain" ~ "smd",
          # standardised mean difference
          outcome == "mood_depression" ~ "smd"
        )
      )
  ),
  
  # useful to have which outcomes we currently include in the analysis
  tar_target(outcomes,
             model_key %>% pull(outcome)),
  
  tar_target(
    metapar_consult,
    raw_covidence_export %>%
      clean_names() %>%
      rename(title = comments) %>%
      select(
        study_identifier,
        title,
        arm = intervention,
        type = intervention_type,
        class = intervention_class,
        name = intervention_name,
        chronic_condition = chronic_pain_condition_s,
        main_aim = main_aim_pain_mood_quality_of_life_etc
        
      ) %>%
      mutate(across(everything(), tolower))
  ),
  
  tar_target(
    export_condition,
    metapar_consult %>%
      select(chronic_condition) %>%
      mutate(
        condition_no_chronic = str_replace_all(chronic_condition, "\\s*chronic\\s*", "")
      ) %>%
      distinct() %>%
      arrange(chronic_condition) %>%
      write_csv("data/chronic_condition.csv")
  ),
  
  
  tar_target(
    metapar_original_study,
    metapar_consult %>%
      left_join(classifiers, by = "chronic_condition") %>%
      mutate(
        type = if_else(type == "placeco", "placebo", type),
        condition = if_else(is.na(condition), chronic_condition, condition),
        name = case_when(
          is.na(name) &
            type == "antidepressant" &
            str_detect(study_identifier, "tasmuth") ~ "venlafaxine",
          is.na(name) & type == "placebo" ~ "placebo",
          TRUE ~ name
        )
      ) %>%
      select(-chronic_condition,-condition_no_chronic)
  ),
  
  tar_target(drug_names_unlabelled,
             metapar %>%
               filter(is.na(name))),
  
  tar_target(
    scales,
    scales_raw %>%
      mutate(across(everything(), tolower)) %>%
      mutate(scale_raw = scale) %>%
      clean_colvals(scale) %>%
      mutate(aka = strsplit(aka, split = "\\s*;\\s*")) %>%
      unnest(aka) %>%
      mutate(
        aka_clean = str_replace_all(aka, "[\\(\\)\\[\\]]", "") %>%
          str_replace_all("[\\s-]", "_")
      ) %>%
      mutate(scale_category = scale) %>%
      rename(primary = scale, secondary = aka_clean) %>%
      pivot_longer(
        cols = c(primary, secondary),
        names_to = "level",
        values_to = "scale"
      )
    
    
  ),
  
  
  
  # study labels ------------------------------------------------------------
  
  tar_target(
    study_lab_metapar,
    metapar_original_study %>%
      mutate(across(everything(), tolower)) %>%
      study_id()
    
  ),
  
  
  tar_target(
    study_key,
    study_lab_metapar %>%
      select(study_identifier, study, title) %>%
      distinct()
  ),
  
  tar_target(
    # set up a check that all study keys got labelled with something
    study_key_assert_na,
    assertthat::assert_that(all(!is.na(study_key$study)),
                            msg = "NAs found in study variable")
  ),
  
  tar_target(study_na_inspect,
             study_key %>%
               filter(is.na(study))),
  
  
  tar_target(
    wo_study,
    raw_outcome_dat %>%
      pluck(outcomes) %>%
      clean_names() %>%
      mutate(across(everything(), tolower)) %>%
      select(-intervention_type,-intervention_name) %>%
      rename(title = comments, arm = intervention) %>%
      left_join(study_key, by = c("study_identifier", "title")) %>%
      # select(-study_identifier, -title) %>%
      select(study, arm, everything())
    ,
    pattern = map(outcomes),
    iteration = "list"
  ),
  
  tar_target(
    unmatched_studies_obs,
    wo_study %>%
      map_df(select, study, study_identifier, title) %>%
      filter(is.na(study)) %>%
      left_join(metapar_original_study, by = c("study_identifier")) %>%
      rename(
        title_outcome_obs = title.x,
        title_raw_covidence = title.y
      ) %>%
      select(study_identifier, title_outcome_obs, title_raw_covidence) %>%
      distinct()
  ),
  
  tar_target(
    # confirm only unique study identifiers have unmatched titles
    # in which case we can simply replace values from the unmatched
    assert_unmatched_studies_unique,
    assert_that(
      (
        unmatched_studies_obs %>%
          pull(study_identifier) %>%
          n_distinct()
      ) == nrow(unmatched_studies_obs),
      msg = "Unmatched studies are not unique"
    )
    
  ),
  
  
  tar_target(
    metapar,
    study_lab_metapar %>%
      select(-title, -study_identifier)
  ),
  
  # wrangle outcomes --------------------------------------------------------
  
  
  tar_target(
    wo_long,
    wo_study %>%
      select(-study_identifier,-title) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = -c(outcome, study, arm),
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
    pattern = map(wo_study)
  ),
  
  tar_target(
    wo_wide,
    wo_long %>%
      pivot_wider(
        id_cols = c(outcome, study, arm, measure_desc),
        names_from = measure_type,
        values_from = covidence_value
      ) %>%
      mutate(across(any_of(
        c("mean", "sd", "se", "percent", "median")
      ), as.numeric)) %>%
      mutate(n = as.integer(n))
  ),
  
  
  
  tar_target(
    wo_time,
    wo_wide %>%
      dplyr::mutate(
        timepoint = dplyr::case_when(
          str_detect(study, "engel 1998") ~ "post_int",
          str_detect(study, "johansson") ~ "post_int",
          str_detect(study, "ginsberg") &
            str_detect(measure_desc, "halfway") ~ "mid_int",
          str_detect(study, "grace 1985") &
            str_detect(measure_desc, "week_[48]") ~ "mid_int",
          str_detect(study, "leijon") &
            str_detect(measure_desc, "week_[123]") ~ "mid_int",
          str_detect(study, "leijon") &
            str_detect(measure_desc, "week_4") ~ "post_int",
          outcome == "adverse" &
            str_detect(study, "agger") ~ "post_int",
          outcome == "pain" &
            str_detect(study, "bansal 2009") ~ "post_int",
          str_detect(study, "nct 2003") &
            str_detect(measure_desc, "8_weeks|week_8") ~ "post_int",
          str_detect(study, "nct 2003") &
            str_detect(measure_desc, "months") ~ "follow_up",
          str_detect(measure_desc, "follow_up") ~ "follow_up",
          stringr::str_detect(
            measure_desc,
            "end_of_treatment|post_treatment|post_intervention|endpoint|ednpoint"
          ) ~ "post_int",
          str_detect(measure_desc, "mid_intervention|within_trial") ~ "mid_int",
          str_detect(measure_desc, "baseline") ~ "baseline",
          TRUE ~ "unmatched"
        )
      ) %>%
      select(outcome, study, arm, measure_desc, timepoint, everything())
  ),
  
  tar_target(
    wo_time_unmatched,
    wo_time %>%
      filter(timepoint == "unmatched") %>%
      select(outcome, study, arm, measure_desc, timepoint) %>%
      distinct()
  ),
  
  tar_target(
    wo_scale_matches,
    wo_time %>%
      mutate(
        matched_scales = map2_chr(measure_desc, outcome, scale_match, scales_df = scales)
      )
  ),
  
  tar_target(
    wo_scale,
    wo_scale_matches %>%
      filter(
        matched_scales != "multiple matches",
        matched_scales != "no matches"
      )
  ),
  
  tar_target(
    wo_scale_multiple_matches,
    wo_scale_matches %>%
      filter(matched_scales == "multiple matches")
  ),
  
  tar_target(
    wo_scale_unmatched,
    wo_scale_matches %>%
      filter(matched_scales == "no matches")
  ),
  
  tar_target(
    scale_match_test,
    scale_match(wo_time$measure_desc[[500]], wo_time$outcome[[500]], scales_df = scales)
  ),
  
  tar_target(obs,
             wo_scale %>%
               separate(
                 study_arm, into = c("study", "arm"), sep = "\\s*=\\s*"
               )),
  
  # filters -----------------------------------------------------------------
  
  tar_target(# export to gs to check with Hollie
    # checking antidepressant vs other antidepressants, etc.
    class_check_gs,
    metapar %>%
      select(study, arm, type, class)),
  
  tar_target(obs_post_int,
             obs %>%
               filter(timepoint == "post_int")),
  
  tar_target(
    obs_ad_post_int,
    metapar %>%
      filter(type == "antidepressant") %>%
      select(study) %>%
      left_join(obs_post_int, by = "study") %>%
      left_join(metapar, by = c("study", "arm")) %>%
      filter(type == "antidepressant" | type == "placebo")
    
  ),
  
  
  # quality and assurance on data -------------------------------------------
  
  # see issue 20
  # tar_target(
  #   issue_20,
  #
  # ),
  
  
  # summary tables and vis --------------------------------------------------
  
  tar_target(
    variables_tab,
    variables %>%
      select(variable, role, description, covidence) %>%
      mutate(across(everything(), as.character)) %>%
      mutate(
        covidence = if_else(
          str_detect(covidence, "[A-Z]"),
          glue("`{covidence}`"),
          covidence
        ),
        variable = glue("`{variable}`")
      ) %>%
      gt(groupname_col = "role") %>%
      hpp_tab(vertical_divider = variable) %>%
      cols_label(
        variable = "Variable",
        description = "Description",
        covidence = "Covidence"
      ) %>%
      fmt_markdown(columns = c("covidence", "variable"))
  ),
  
  # models ------------------------------------------------------------------
  
  tar_target(
    model_metapar,
    # create a tibble of all possible combinations?
    # then filter?
    # or simply write a model function that boots out to an NA if it's not there?
    # start by finding out how many metaparameters we're looking at
    # inspect the table
    # filters
    # subgroup
    # variable
    list(
      outcome = outcomes,
      
      # filters
      timepoint = "post_int",
      design = "parallel",
      
      # subgroups
      # using all to denote all values of that variable are to be included in
      # filters
      condition = metapar %>% pull(general_grouping) %>% unique() %>% c("all", .),
      class  = metapar %>% pull(class) %>% unique() %>% c("all", .),
      main_aim = metapar %>% pull(main_aim) %>% unique() %>% c("all", .)
    ) %>%
      cross_df()
  ),
  
  
  # update site -------------------------------------------------------------
  
  # tar_target(
  #   update_site,
  #   rmarkdown::render_site(preview = FALSE)
  # ),
  
  # this is just here so I don't have to worry about final commas
  # when testing
  NULL
)
