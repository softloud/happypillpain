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

# so I can run specific parts of the pipeline with starts with *_

# starts with phases

# r := raw
# w := wrangling
# m := model
# c := check, for exporting checks and asserts
# e := exploratory data analysis table or vis
# h := prepared by hollie

# other shorthands


list(
  # raw ---------------------------------------------------------------------
  
  # raw data observations ------------------------------------------------------------
  
  # see data-raw for origin of these files and from which gs they were scraped
  
  tar_target(r_outcome_adverse,
             read_csv("data/Adverse_Events.csv")),
  
  tar_target(r_outcome_pain,
             read_csv("data/Pain_intensity.csv")),
  
  tar_target(r_outcome_mood,
             read_csv("data/Mood.csv")),
  
  tar_target(
    r_outcome_obs,
    list(
      adverse = r_outcome_adverse %>% mutate(outcome = "adverse"),
      pain = r_outcome_pain %>% mutate(outcome = "pain"),
      mood_depression = r_outcome_mood %>% mutate(outcome = "mood_depression")
    )
  ),
  
  tar_target(
    r_covidence,
    read_csv(
      "data/review_91309_extracted_data_csv_20210519211840.csv",
      col_types = cols(.default = "c")
    )
  ),
  
  
  # raw labels --------------------------------------------------------------
  
  # labels provided by hollie
  tar_target(r_classifiers,
             read_csv("data/classifiers.csv")),
  
  tar_target(r_variables,
             read_csv("data/variables.csv")),
  
  tar_target(r_scales,
             read_csv("data/scales.csv") %>% clean_names()),
  
  
  
  # raw constructed ---------------------------------------------------------
  
  
  # assign measure of comparison for each outcome
  tar_target(
    r_model_key,
    # make sure this matches raw outcomes
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
  tar_target(r_outcomes,
             r_model_key %>% pull(outcome)),
  
  
  # wrangling ---------------------------------------------------------------
  
  
  # metaparameters ----------------------------------------------------------
  
  tar_target(
    w_metapar_consult,
    r_covidence %>%
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
        main_aim = main_aim_pain_mood_quality_of_life_etc,
        design = group
        
      ) %>%
      mutate(across(everything(), tolower))
  ),
  
  tar_target(
    c_export_condition,
    w_metapar_consult %>%
      select(chronic_condition) %>%
      mutate(
        condition_no_chronic = str_replace_all(chronic_condition, "\\s*chronic\\s*", "")
      ) %>%
      distinct() %>%
      arrange(chronic_condition) %>%
      write_csv("data/chronic_condition.csv")
  ),
  
  
  tar_target(
    w_metapar_original_s,
    w_metapar_consult %>%
      left_join(r_classifiers, by = "chronic_condition") %>%
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
  
  tar_target(c_drug_names_unlabelled,
             metapar %>%
               filter(is.na(name))),
  
  tar_target(
    w_scales,
    r_scales %>%
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
      ) %>%
      mutate(scale_category = str_replace(scale_category, "x0", "0")) %>%
      mutate(outcome = if_else(
        str_detect(outcome, "mood"), "mood_depression", outcome
      ))
    
  ),
  
  
  
  
  # study labels ------------------------------------------------------------
  
  tar_target(
    w_label_s,
    w_metapar_original_s %>%
      mutate(across(everything(), tolower)) %>%
      study_id()
    
  ),
  
  
  tar_target(
    w_s_key,
    w_label_s %>%
      select(study_identifier, study, title) %>%
      distinct()
  ),
  
  tar_target(
    # set up a check that all study keys got labelled with something
    w_s_key_assert_na,
    assertthat::assert_that(all(!is.na(w_s_key$study)),
                            msg = "NAs found in study variable")
  ),
  
  tar_target(c_s_na,
             w_s_key %>%
               filter(is.na(study))),
  
  
  tar_target(
    w_s_with_unmatched,
    r_outcome_obs %>%
      pluck(r_outcomes) %>%
      clean_names() %>%
      mutate(across(everything(), tolower)) %>%
      select(-intervention_type,-intervention_name) %>%
      rename(title = comments, arm = intervention) %>%
      left_join(w_s_key, by = c("study_identifier", "title")) %>%
      # select(-study_identifier, -title) %>%
      select(study, arm, everything())
    ,
    pattern = map(r_outcomes),
    iteration = "list"
  ),
  
  
  
  tar_target(
    c_unmatched_s_obs,
    w_s_with_unmatched %>%
      map_df(select, study, study_identifier, title) %>%
      filter(is.na(study)) %>%
      left_join(w_metapar_original_s, by = c("study_identifier")) %>%
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
    c_assert_unmatched_s_unique,
    assert_that(
      (unmatched_s_obs %>%
         pull(study_identifier) %>%
         n_distinct()) == nrow(unmatched_studies_obs),
      msg = "Unmatched studies are not unique"
    )
    
  ),
  
  
  tar_target(
    metapar,
    w_label_s %>%
      select(-study_identifier) %>%
      # select(everything(), title) %>%
      mutate(title = str_replace(title, "title: ", ""))
  ),
  
  tar_target(
    w_s,
    w_s_w_unmatched %>%
      filter(
        !is.na(study),!any(study %in% unmatched_studies_obs$study_identifier)
      ),
    pattern = map(w_s_w_unmatched)
    
  ),
  
  # wrangle outcomes --------------------------------------------------------
  
  
  tar_target(
    w_long,
    w_s %>%
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
    pattern = map(w_s)
  ),
  
  tar_target(
    w_wide,
    w_long %>%
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
    w_time,
    w_wide %>%
      dplyr::mutate(
        # specific study changes
        timepoint = dplyr::case_when(
          # see issue # 26
          str_detect(study, "pirbudak 2003") &
            str_detect(measure_desc, "9_months") ~ 'post_int',
          str_detect(study, "pirbudak 2003") &
            str_detect(measure_desc, "2_weeks|6_weeks|3_months|6_month") ~ 'mid_int',
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
          
          # general labels
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
    w_time_unmatched,
    w_time %>%
      filter(timepoint == "unmatched") %>%
      select(outcome, study, arm, measure_desc, timepoint) %>%
      distinct()
  ),
  
  tar_target(w_scale_matches,
             w_time %>%
               mutate(
                 matched_scales = map2_chr(measure_desc, outcome, scale_match, scales_df = scales)
               )),
  
  tar_target(
    w_scale,
    w_scale_matches %>%
      filter(
        matched_scales != "multiple matches",
        matched_scales != "no matches"
      )
  ),
  
  tar_target(
    w_scale_multiple_matches,
    w_scale_matches %>%
      filter(matched_scales == "multiple matches")
  ),
  
  tar_target(
    w_scale_unmatched,
    w_scale_matches %>%
      filter(matched_scales == "no matches")
  ),
  
  tar_target(
    scale_match_test,
    scale_match(w_time$measure_desc[[500]], w_time$outcome[[500]], scales_df = scales)
  ),
  
  tar_target(obs,
             w_scale %>%
               left_join(metapar, by = c("study", "arm"))),
  
  
  tar_target(# export to gs to check with Hollie
    # checking antidepressant vs other antidepressants, etc.
    class_check_gs,
    metapar %>%
      select(study, arm, type, class)),
  
  
  # studies with multiple outcomes ------------------------------------------
  
  tar_target(
    s_multiple_outcomes,
    obs %>%
      count(study, title, outcome) %>%
      group_by(study) %>%
      arrange(study) %>%
      mutate(
        n_outcomes = 1:n(),
        max_n_outcomes = max(n_outcomes)
      ) %>%
      filter(max_n_outcomes > 1) %>%
      select(-n,-n_outcomes,-max_n_outcomes) %>%
      count(study, title, outcome) %>%
      pivot_wider(
        id_cols = c(study, title),
        values_from = n,
        names_from = outcome
      ) %>%
      arrange(adverse, mood_depression, pain) %>%
      # mark as true if NA
      mutate(across(any_of(outcomes), is.na)) %>%
      # but actually want to know which ones aren't NA
      mutate(across(any_of(outcomes), isFALSE))
  ),
  
  tar_target(
    tab_s_multiple_outcomes,
    s_multiple_outcomes %>%
      ungroup() %>%
      gt() %>%
      hpp_tab(vertical_divider = title) %>%
      data_color(
        columns = all_of(outcomes),
        colors = scales::col_factor(palette = c("white", "#d9e6f2"),
                                    domain = NULL)
      )
  ),
  
  
  # studies with multiple scales --------------------------------------------
  
  tar_target(s_multiple_scales,
             obs %>%
               count(study, arm, scale)),
  
  # summary tables and vis --------------------------------------------------
  
  tar_target(
    e_variables,
    r_variables %>%
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
      hpp_tab(vertical_divider = "variable") %>%
      cols_label(
        variable = "Variable",
        description = "Description",
        covidence = "Covidence"
      ) %>%
      fmt_markdown(columns = c("covidence", "variable")) %>%
      tab_footnote(
        "Variable column indicates what the cleaned output data
                   column headers are, the Covidence column indicates what the
                   column is labelled as in the raw export."
      )
  ),
  
  # models ------------------------------------------------------------------
  
  tar_target(
    m_metapar,
    # create a tibble of all possible combinations?
    # then filter?
    # or simply write a model function that boots out to an NA if it's not there?
    # start by finding out how many metaparameters we're looking at
    # inspect the table
    # filters
    # subgroup
    # variable
    list(
      outcome = r_outcomes,
      
      # filters
      timepoint = "post_int",
      design = "parallel group",
      
      # subgroups
      # using all to denote all values of that variable are to be included in
      # filters
      condition = metapar %>%
        pull(general_grouping) %>%
        unique(),
      class  = metapar %>%
        pull(class) %>%
        unique(),
      main_aim = metapar %>%
        pull(main_aim) %>%
        unique()
    ) %>%
      cross_df()
  ),
  
  # all in
  tar_target(
    m_all_in,
    m_metapar %>%
      filter(outcome == r_outcomes) %>%
      select(-condition, -class, -main_aim) %>%
      distinct() %>%
      left_join(obs),
    pattern = map(r_outcomes),
    iteration = "list"
    
  ),
  
  
  # parameterised reports ---------------------------------------------------
  
  # tar_target(
  #   render_outcome_reports,
  #
  #   render(
  #     input = "template-outcome.Rmd",
  #     params = list(outcome = outcomes),
  #     output_file = glue("outcome-{outcomes}.html"),
  #     output_dir = "docs"
  #   ),
  #   pattern = map(outcomes)
  # ),
  #
  # tar_target(render_hpp,
  #            render_site()),
  
  # this is just here so I don't have to worry about final commas
  # when testing
  NULL
)
