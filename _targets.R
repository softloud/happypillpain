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
  library(multinma)
  
  conflicted::conflict_prefer("filter", "dplyr")
  
})

installed_pkg <- installed.packages()[, 1]

if ("dontpanic" %in% installed_pkg) {
  require(dontpanic)
}




# functions ---------------------------------------------------------------
c(
  "study_id",
  "lotr_study_hash",
  "hpp_net",
  "hpp_themes",
  "viable_observations",
  "scale_match"
) %>%
  paste0("R/", ., ".R") %>%
  map(source)

safe_nma <- safely(nma, otherwise = "failed")

# shorthand ---------------------------------------------------------------

# run specific parts of the pipeline: starts_with *_

# starts_with labels

# r := raw
# w := wrangling
# m := model
# c := check, for exporting checks and asserts
# e := exploratory data analysis table or vis
# p := pipeline design
# o: = write outputs

# secondary signifiers ^_*_

# h := prepared by hollie


list(
  # pipeline design ---------------------------------------------------------
  
  tar_target(test_empty_tar,
             NULL),
  
  tar_target(
    p_metapar,
    tibble(
      study = "unique study identifier",
      arm = "unique arm identifier",
      type = "Intervention Type",
      design = "Group",
      condition = "Chronic pain conditions(s)",
      class = "Intervention Class",
      main_aim = "Main aim (Pain..."
    )
  ),
  
  tar_target(
    p_metapar_tab,
    p_metapar %>%
      gt() %>%
      hpp_tab(vertical_divider = "arm") %>%
      tab_header("Study-level information drawn from Covidence")
  ),
  
  tar_target(
    p_obs,
    tibble(
      outcome = "mood, pain, etc.",
      study = p_metapar %>% pull(study),
      arm = p_metapar %>% pull(arm),
      obs = "columns with mean, sd, counts, sample size, etc.",
      obs_info = "information extracted from column headers:
        timepoint, scale, etc."
    )
  ),
  
  tar_target(
    p_obs_tab,
    p_obs  %>%
      gt() %>%
      hpp_tab(vertical_divider = "arm") %>%
      tab_header("Outcome-level observational from data extracted by Hollie")
  ),
  
  
  tar_target(p_output_raw,
             p_obs %>%
               left_join(p_metapar, by = c("study", "arm"))),
  
  
  tar_target(
    p_output,
    p_output_raw %>%
      gt() %>%
      hpp_tab(vertical_divider = "arm") %>%
      tab_header("Each row provides observations for one arm of one study",
                 subtitle = "Study-level and observational variables included")
  ),
  
  # variables ---------------------------------------------------------------
  
  # table with covidence column names and preferred column names
  tar_target(
    r_variables,
    read_csv("data/variables-2021-07-12_16:37:56.csv")
  ),
  
  # output table of variables
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
      tab_source_note(
        "Variable column indicates what the cleaned output data
                     column headers are, the Covidence column indicates what the
                     column is labelled as in the raw export."
      )
  ),
  
  
  
  
  
  
  # covidence export --------------------------------------------------------
  
  tar_target(
    r_covidence,
    #suppressWarnings(suppressMessages(
    read_csv("data/review_91309_extracted_data_csv_20210820014615.csv")
    #))
  ),
  
  tar_target(
    w_covidence_cleaned,
    r_covidence %>%
      clean_names() %>%
      select(
        study_covidence = study_identifier,
        title_covidence = comments,
        everything()
      )
  ),
  
  
  
  # hollie's extractions ----------------------------------------------------
  
  tar_target(r_h_outcome_adverse,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-08-16/Adverse Events.csv")
             ))),
  
  tar_target(r_h_outcome_mood,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-08-16/Mood.csv"),
             ))),
  
  tar_target(r_h_outcome_pain_int,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-08-16/Pain intensity.csv")
             ))),
  
  # bring in the new stuff
  # the 2021-07-07 h_ exports have the same number of rows
  tar_target(r_h_outcome_pain_mod,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-08-16/Moderate pain relief.csv")
             ))),
  
  tar_target(r_h_outcome_physical,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-08-16/Physical function.csv")
             ))),
  
  tar_target(r_h_outcome_qol,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-08-16/Quality of life.csv")
             ))),
  
  tar_target(r_h_outcome_sleep,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-08-16/Sleep.csv")
             ))),
  
  tar_target(r_h_outcome_withdrawal,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-08-16/Withdrawal.csv")
             ))),
  
  tar_target(r_h_outcome_pain_sub,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-08-16/Substantial pain relief.csv")
             ))),
  
  # put all observations in one list
  tar_target(
    r_h_outcome_obs,
    list(
      mood = r_h_outcome_mood,
      pain_int = r_h_outcome_pain_int,
      adverse = r_h_outcome_adverse,
      physical = r_h_outcome_physical,
      qol = r_h_outcome_qol,
      sleep = r_h_outcome_sleep,
      pain_mod = r_h_outcome_pain_mod,
      withdrawal = r_h_outcome_withdrawal,
      pain_sub = r_h_outcome_pain_sub
    )
    
  ),
  
  tar_target(w_outcomes,
             r_h_outcome_obs %>% names()),
  
  tar_target(
    w_outcome_obs,
    r_h_outcome_obs %>%
      pluck(w_outcomes) %>%
      # tidy up column names
      clean_names %>%
      # remove columns we'll pull from the study_arm df
      select(-any_of(
        c("intervention_name", "intervention_type")
      )) %>%
      rename(arm = intervention) %>%
      # add outcome
      mutate(outcome = w_outcomes) %>%
      select(outcome,
             study_h = study_identifier,
             title_h = comments,
             everything()),
    pattern = map(w_outcomes),
    iteration = "list"
  ),
  
  
  # take hollie's extractions long ------------------------------------------
  tar_target(
    w_obs_long,
    w_outcome_obs %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = -c(outcome, study_h, arm, title_h),
        names_to = "covidence_colname",
        values_to = "covidence_value",
        values_drop_na = TRUE
      ) %>%
      mutate(
        measure_matches =
          map(covidence_colname, str_match, "(.+)_([a-z]+)[_\\d]*$"),
        measure_type = map_chr(measure_matches, 3),
        covidence_desc = map_chr(measure_matches, 2)
      ) %>%
      select(-measure_matches, -covidence_colname) %>%
      mutate(across(everything(), tolower)) %>%
      left_join(m_key, by = "outcome")
    ,
    pattern = map(w_outcome_obs)
  ),
  
  
  
  # model types -------------------------------------------------------------
  
  tar_target(
    m_key,
    tibble(outcome = w_outcomes) %>%
      mutate(
        model_type = case_when(
          outcome == "adverse" ~ "lor",
          outcome == "pain_mod" ~ "lor",
          outcome == "pain_sub" ~ "lor",
          outcome == "withdrawal" ~ "lor",
          TRUE ~ "smd"
        )
      )
  ),
  
  # study labels ------------------------------------------------------------
  
  # create study key with study identifier, title, and unique study id
  
  tar_target(
    w_study_key,
    w_covidence_cleaned %>%
      select(study_covidence, title_covidence) %>%
      distinct() %>%
      arrange(study_covidence) %>%
      study_id() %>%
      mutate(
        title = str_replace(title_covidence, "Title: ", ""),
        study = as.character(study)
      ) %>%
      mutate(across(everything(), tolower))
  ),
  
  # now we have a study key we can instantiate study_arm info
  tar_target(
    w_covidence,
    w_covidence_cleaned %>%
      # apply study labels
      left_join(w_study_key, by = c("study_covidence", "title_covidence")) %>%
      mutate(across(everything(), tolower)) %>%
      select(study, everything(), study_covidence, title_covidence)
  ),
  
  # apply study labels to observations
  tar_target(
    w_obs_study,
    w_obs_long %>%
      left_join(
        w_study_key,
        by = c("study_h" = "study_covidence",
               "title_h" = "title_covidence")
      ) %>%
      select(outcome, study, arm, everything())
  ),
  # study unmatched ---------------------------------------------------------
  
  tar_target(
    w_study_unmatched,
    w_obs_study %>%
      filter(is.na(study)) %>%
      select(study_h, title_h) %>%
      distinct()
  ),
  
  tar_target(
    w_study_unmatched_studies,
    w_study_unmatched %>% pull(study_h)
  ),
  
  tar_target(
    w_study_unmatched_key,
    w_study_key %>%
      filter(study_covidence %in% w_study_unmatched_studies)
  ),
  
  # as the studis that are unmatched have only a single, we can join on study only
  tar_target(
    w_obs_study_fix,
    w_obs_study %>%
      mutate(
        study = map2_chr(
          study,
          study_h,
          .f = function(s, h) {
            if (is.na(s)) {
              w_study_unmatched_key %>%
                filter(study_covidence == h) %>% pull(study)
            } else
              s
          }
        ),
        title = map2_chr(
          study_h,
          title,
          .f = function(h, t) {
            if (is.na(t)) {
              w_study_unmatched_key %>%
                filter(study_covidence == h) %>% pull(title)
            } else
              t
          }
        )
      )
    
  ),
  
  tar_target(w_study_assert, {
    count_unmatched <-
      w_obs_study_fix %>%
      filter(is.na(study) | is.na(title)) %>%
      nrow()
    
    assert_that(count_unmatched == 0,
                msg = "NAs in either study or title label")
  }),
  
  
  # go wide -----------------------------------------------------------------
  
  tar_target(
    w_obs_wide,
    w_obs_study_fix %>%
      select(-study_h,-title_h) %>%
      pivot_wider(names_from = measure_type,
                  values_from = covidence_value)
  ),
  # timepoints --------------------------------------------------------------
  
  tar_target(
    w_obs_time,
    w_obs_wide %>%
      dplyr::mutate(
        # specific study changes
        timepoint = dplyr::case_when(
          # see issue # 26
          str_detect(study, "pirbudak 2003") &
            str_detect(covidence_desc, "9_months") ~ 'post_int',
          str_detect(study, "pirbudak 2003") &
            str_detect(covidence_desc, "2_weeks|6_weeks|3_months|6_month") ~ 'mid_int',
          str_detect(study, "engel 1998") ~ "post_int",
          str_detect(study, "johansson") ~ "post_int",
          str_detect(study, "ginsberg") &
            str_detect(covidence_desc, "halfway") ~ "mid_int",
          str_detect(study, "grace 1985") &
            str_detect(covidence_desc, "week_[48]") ~ "mid_int",
          str_detect(study, "leijon") &
            str_detect(covidence_desc, "week_[123]") ~ "mid_int",
          str_detect(study, "leijon") &
            str_detect(covidence_desc, "week_4") ~ "post_int",
          outcome == "adverse" &
            str_detect(study, "agger") ~ "post_int",
          outcome == "pain" &
            str_detect(study, "bansal 2009") ~ "post_int",
          str_detect(study, "nct 2003") &
            str_detect(covidence_desc, "8_weeks|week_8") ~ "post_int",
          str_detect(study, "nct 2003") &
            str_detect(covidence_desc, "months") ~ "follow_up",
          
          # general labels
          str_detect(covidence_desc, "follow_up") ~ "follow_up",
          stringr::str_detect(
            covidence_desc,
            "end_of_treatment|post_treatment|post_intervention|endpoint|ednpoint"
          ) ~ "post_int",
          str_detect(covidence_desc, "mid_intervention|within_trial") ~ "mid_int",
          str_detect(covidence_desc, "baseline") ~ "baseline",
          TRUE ~ "unmatched"
        )
      ) %>%
      select(outcome, study, arm, covidence_desc, timepoint, everything())
  ),
  
  # wrangle scales ----------------------------------------------------------

  tar_target(r_scales,
             read_rds("data/scales-2021-08-18_13:56:22.rds")),

  tar_target(
    w_scales,
    r_scales %>%
      clean_names() %>%
      rename(scale_category = scale) %>%
      mutate(
        outcome_label = outcome,
        scale_label = scale_category,
        scale_category = tolower(scale_category),
        aka = tolower(aka),
        outcome = tolower(outcome),
        outcome =
          case_when(
            str_detect(outcome, "mood") ~ "mood",
            outcome == "pain" ~ "pain_int",
            str_detect(outcome, "physical") ~ "physical",
            str_detect(outcome, "quality") ~ "qol",
            TRUE ~ outcome
          )
      ) %>%
      mutate(aka = strsplit(aka, split = "\\s*;\\s*"))  %>%
      unnest(aka) %>%
      # clean the aka column so it's the same as the cleaned janitor names
      mutate(
        # store raw aka for troubleshooting
        aka_raw = aka,
        # remove whitespace from start and end
        aka = str_trim(aka),
        aka =
          # replace () and [] with nothing
          str_replace_all(aka, "[\\(\\)\\[\\]]", "") %>%
          # replace - and space with _
          str_replace_all("[\\s+-]", "_")
      ) %>%
      mutate(
        scale_category =
          str_replace_all(scale_category, "[\\s+\\-,]", "_") %>%
          str_remove_all("[\\(\\)]") %>%
          str_replace_all("_+", "_")
      )

  ),

  # scale matches -----------------------------------------------------------

  tar_target(
    w_obs_scale_matches,
    w_obs_time %>%
      mutate(scale_match = pmap(
        list(outcome,
             covidence_desc,
             model_type),
        .f = scale_match,
        scale_df = w_scales
      ))
  ),



  tar_target(
    w_obs_scale_counts,
    w_obs_scale_matches %>%
      # select(study, outcome, covidence_desc, scale_match) %>%
      mutate(cat_n = map_int(
        scale_match,
        .f = function(df) {
          df %>%
            select(scale_category) %>%
            distinct() %>%
            nrow()
        }
      ))
  ),

  tar_target(w_obs_scale_snapshot,
             w_obs_scale_counts %>%
               count(cat_n)),

  tar_target(w_obs_scale_excluded,
             w_obs_scale_counts %>%
               filter(cat_n != 1)),

  tar_target(
    w_obs_scale_excluded_next,
    w_obs_scale_excluded %>%
      filter(cat_n != 1) %>%
      filter(!str_detect(covidence_desc, "0_10_cm_vas")) %>%
      head(1) %>%
      select(outcome, covidence_desc, scale_match)
  ),



  tar_target(
    w_obs_scales_viable,
    w_obs_scale_counts %>%
      filter(cat_n == 1) %>%
      mutate(scale = map_chr(
        scale_match,
        .f = function(df) {
          df %>% select(scale_category) %>%
            distinct() %>% pull(scale_category)
        }
      )) %>%
      select(-scale_match,-cat_n)
  ),

  tar_target(w_obs_scales,
             w_obs_scales_viable),




  
  
  # # wrangle study-arm labels ------------------------------------------------
  #
  # tar_target(
  #   r_condition,
  #   read_csv("data/conditions-2021-07-12_16:37:56.csv")
  # ),
  #
  # tar_target(
  #   w_condition,
  #   r_condition %>%
  #     select(-condition_no_chronic) %>%
  #     rename(condition_general = general_grouping,
  #            condition_iasp = iasp_classification)
  # ),
  #
  #
  # tar_target(
  #   w_par_labels,
  #   w_covidence  %>%
  #     select(
  #       study,
  #       intervention,
  #       title,
  #       intervention_type,
  #       intervention_class,
  #       intervention_name,
  #       group,
  #       sponsorship_source,
  #       authors_name,
  #       duration_weeks,
  #       pain_duration,
  #       inclusion_criteria,
  #       exclusion_criteria,
  #       starts_with("main_aim"),
  #       starts_with("chronic_pain"),
  #       starts_with("total_number_of_participants"),
  #       starts_with("number_in_each_arm"),
  #       starts_with("type_of_participant"),
  #       starts_with("intervention_adverse"),
  #       starts_with("withdrawal_total")
  #     ) %>%
  #     # rename variables
  #     select(
  #       study,
  #       arm = intervention,
  #       intervention = intervention_name,
  #       type = intervention_type,
  #       class = intervention_class,
  #       condition = chronic_pain_condition_s,
  #       main_aim = main_aim_pain_mood_quality_of_life_etc,
  #       design = group,
  #       everything()
  #     ) %>%
  #     rename(chronic_condition = condition) %>%
  #     left_join(w_condition, by = "chronic_condition") %>%
  #     mutate(
  #       # label placebo interventions
  #       intervention = if_else(is.na(intervention) &
  #                                type == "placebo",
  #                              "placebo",
  #                              intervention),
  #       # input missing interventions
  #       intervention =
  #         str_remove_all(arm, "[\\d*.*\\d|mg|-]") %>%
  #         str_remove_all("\\s{2,}|\\s$"),
  #
  #       # classes
  #
  #       # fix a spelling mistake in classes
  #       class = if_else(str_detect(class, "tetra"),
  #                       "tetracyclic (teca)",
  #                       class),
  #
  #       # one of the placebo classes is labelled "n"
  #       class = ifelse(class == "n", NA, class)
  #     )
  #
  # ),
  #
  #
  # # dose --------------------------------------------------------------------
  #
  #
  # tar_target(
  #   w_par_dose,
  #   w_par_labels %>%
  #     mutate(dose_extract = map(arm, str_extract_all, "\\d+\\w*")) %>%
  #     select(dose_extract, everything())
  #
  # ),
  #
  #
  # # set study-level parameters ----------------------------------------------
  #
  #
  # tar_target(w_study_arm_par,
  #            w_par_dose),
  #
  #

  # # observations ------------------------------------------------------------
  #
  #
  #
  # # filters -----------------------------------------------------------------
  #
  # # currently filtering to neuropathic and scales categorised to 1
  # # in order to filter, need study-level things
  #
  # tar_target(
  #   w_obs_long_metapar,
  #   w_obs_long %>%
  #     # hack until studies are sorted
  #     filter(!is.na(study)) %>%
  #     left_join(
  #       w_study_arm_par %>%
  #         select(study, arm, condition_general, design)
  #       ,
  #       by = c("study", "arm")
  #     )
  # ),
  #

  # # go wide -----------------------------------------------------------------
  #
  #
  # tar_target(
  #   w_obs_wide_duplicates,
  #   w_obs_scales_viable %>%
  #     pivot_wider(
  #       values_fn = length,
  #       # for debugging
  #       id_cols = c(outcome,
  #                   study,
  #                   arm,
  #                   covidence_desc,
  #                   scale,
  #                   model_type),
  #       names_from = measure_type,
  #       values_from = covidence_value
  #     ) %>%
  #     filter(
  #       mean > 1 |
  #         sd > 1 |
  #         n > 1 |
  #         percent > 1 |
  #         se > 1 |
  #         range > 1 |
  #         median > 1 |
  #         ci > 1 |
  #         intervals > 1 |
  #         iqr > 1 |
  #         limits > 1
  #     )
  # ),
  #
  # tar_target(
  #   w_obs_wide_to_filter,
  #   w_obs_wide_duplicates %>%
  #     select(outcome, study, arm, covidence_desc, scale) %>%
  #     distinct()
  # ),
  #
  # tar_target(
  #   w_obs_wide_viable,
  #   w_obs_scales_viable %>%
  #     anti_join(w_obs_wide_to_filter)
  # ),
  #
  # tar_target(
  #   w_obs_wide,
  #   w_obs_wide_viable %>%
  #     pivot_wider(
  #       id_cols = c(outcome,
  #                   study,
  #                   arm,
  #                   covidence_desc,
  #                   scale,
  #                   model_type),
  #       names_from = measure_type,
  #       values_from = covidence_value
  #     ) %>%
  #     mutate(across(any_of(
  #       c("mean", "sd", "se", "percent", "median")
  #     ), as.numeric)) %>%
  #     mutate(n = as.integer(n))
  # ),
  #
  #
  
  #
  #
  # # bundle everything together ----------------------------------------------
  #
  # # get sds
  # tar_target(
  #   w_obs_calc,
  #   w_obs_time %>%
  #     mutate(
  #       # fix this later
  #       r = ifelse(!is.na(percent), n, NA),
  #       se = if_else(sd > 0 & is.na(se) & n > 0,
  #                    sd / sqrt(n),
  #                    se),
  #       sd = if_else(se > 0 & is.na(sd) & n > 0,
  #                    se * sqrt(n),
  #                    sd),
  #       n = if_else(model_type == "lor" & percent > 0,
  #                   as.integer(r * 100 / percent),
  #                   n)
  #     )
  # ),
  #
  # # todo: target for rows not used
  # tar_target(
  #   w_obs_viable,
  #   w_obs_calc %>%
  #     mutate(
  #       model_viable = case_when(
  #         model_type == "lor" ~ n > 0 & r > 0,
  #         model_type == "smd" ~ mean > 0 & se > 0 & n > 0
  #       )
  #     ) %>%
  #     filter(model_viable) %>%
  #     filter(!is.na(study)) %>%
  #     select(-model_viable)
  # ),
  #
  # tar_target(w_obs_investigate,
  #            anti_join(w_obs_calc, w_obs_viable)),
  #
  # # todo: add filters to observation
  #
  # # keep this target as the final all-in output dataframe to be shared
  # # use this target for EDA
  # tar_target(
  #   w_obs,
  #   w_obs_viable %>%
  #     left_join(w_study_arm_par, by = c("study", "arm")) %>%
  #     rename(covidence = covidence_desc) %>%
  #     select(study, intervention, arm, everything(), covidence) %>%
  #     # select(-range, -median, -ci, -iqr, -percent, -intervals, -model_type) %>%
  #     relocate(r, .before = n) %>%
  #     ungroup() %>%
  #     mutate(
  #       # tidy up intervention
  #       intervention = if_else(
  #         is.na(intervention) | intervention == "n",
  #         str_remove_all(arm, "[^a-z|\\s|+]") %>%
  #           str_remove_all("and mg") %>%
  #           str_remove_all("[mg ]*\\s*$"),
  #         intervention
  #       ),
  #       # create intervention-scale & arm-scale
  #       intervention_scale =
  #         if_else(
  #           intervention == "placebo",
  #           intervention,
  #           str_c(intervention, scale, sep = " x ")
  #         )
  #       ,
  #       arm_scale =
  #         if_else(
  #           intervention == "placebo",
  #           intervention,
  #           str_c(arm, scale, sep = " x ")
  #         )
  #
  #     ) %>%
  #     select(study, intervention_scale, arm_scale, everything())
  # ),
  #
  # tar_target(
  #   w_obs_m,
  #   w_obs  %>%
  #     # apply filters here for now
  #     filter(
  #       timepoint == "post_int",
  #       str_detect(design, "parallel"),
  #       # this is a sledgehammer
  #       type %in% c("placebo", "antidepressant")
  #     )
  #
  # ),
  #
  #
  #
  # # shiny export obs --------------------------------------------------------
  #
  #
  # # tar_target(o_shiny,
  # #            {
  # #              write_rds(w_obs, "hppshiny/obs.rds")
  # #              write_rds(m_key, "hppshiny/model-key.rds")
  # #
  # #            }),
  #
  #
  #
  # # models ------------------------------------------------------------------
  #
  #
  # # all in ------------------------------------------------------------------
  #
  #
  #
  # null --------------------------------------------------------------------
  
  NULL
)
