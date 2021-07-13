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
c("study_id",
  "lotr_study_hash",
  "hpp_net",
  "hpp_themes") %>%
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
  
  
  tar_target(
    p_output,
    p_obs %>%
      left_join(p_metapar, by = c("study", "arm")) %>%
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
  
  tar_target(r_covidence,
             suppressWarnings(suppressMessages(
               read_csv("data/review_91309_extracted_data_csv_20210712234312.csv")
             ))),
  
  tar_target(
    w_covidence,
    r_covidence %>%
      clean_names() %>%
      select(
        study_identifier,
        intervention,
        comments,
        intervention_type,
        intervention_class,
        intervention_name,
        group,
        sponsorship_source,
        authors_name,
        duration_weeks,
        pain_duration,
        inclusion_criteria,
        exclusion_criteria,
        starts_with("main_aim"),
        starts_with("chronic_pain"),
        starts_with("total_number_of_participants"),
        starts_with("number_in_each_arm"),
        starts_with("type_of_participant"),
        starts_with("intervention_adverse"),
        starts_with("withdrawal_total")
      ) %>%
      # rename variables
      select(
        arm = intervention,
        intervention = intervention_name,
        type = intervention_type,
        class = intervention_class,
        condition = chronic_pain_condition_s,
        main_aim = main_aim_pain_mood_quality_of_life_etc,
        design = group,
        everything()
      )
  ),
  
  
  
  # hollie's extractions ----------------------------------------------------
  
  tar_target(r_h_outcome_adverse,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-07-12/Adverse Events.csv")
             ))),
  
  tar_target(r_h_outcome_mood,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-07-12/Mood.csv"),
             ))),
  
  tar_target(r_h_outcome_pain_int,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-07-12/Pain intensity.csv")
             ))),
  
  # bring in the new stuff
  # the 2021-07-07 h_ exports have the same number of rows
  tar_target(r_h_outcome_pain_mod,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-07-12/Moderate pain relief.csv")
             ))),
  
  tar_target(r_h_outcome_physical,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-07-12/Physical function.csv")
             ))),
  
  tar_target(r_h_outcome_qol,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-07-12/Quality of life.csv")
             ))),
  
  tar_target(r_h_outcome_sleep,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-07-12/Sleep.csv")
             ))),
  
  tar_target(r_h_outcome_withdrawal,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-07-12/Withdrawal.csv")
             ))),
  
  tar_target(r_h_outcome_pain_sub,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-07-12/Substantial pain relief.csv")
             ))),
  
  
  # pain mod doesn't have comments column
  # has the same number of rows, will just cbind it
  tar_target(
    w_h_outcome_pain_mod,
    r_h_outcome_withdrawal %>% select(Comments) %>%
      cbind(r_h_outcome_pain_mod)
  ),
  
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
      pain_mod = w_h_outcome_pain_mod,
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
      select(outcome, everything()) %>%
      # title don't quite match, must be an errant symbol for Richards
      mutate(
        comments = if_else(
          str_detect(study_identifier, "Richards 2015") &
            str_detect(comments, "Venlafaxine XR"),
          "Title: Efficacy of Venlafaxine XR for the Treatment of Pain in Patients With Spinal Cord Injury and Major Depression: A Randomized, Controlled Trial",
          comments
        )
      )
    
    ,
    pattern = map(w_outcomes),
    iteration = "list"
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
    w_covidence %>%
      select(study_identifier, comments) %>%
      distinct() %>%
      arrange(study_identifier) %>%
      study_id() %>%
      mutate(title = str_replace(comments, "Title: ", ""))
  ),
  
  # now we have a study key we can instantiate study_arm info
  tar_target(
    w_study_label_study_arm_par,
    w_covidence %>%
      # apply study labels
      left_join(w_study_key, by = c("study_identifier", "comments")) %>%
      select(-study_identifier,-comments) %>%
      select(study, everything())
  ),
  
  # apply study labels to observations
  tar_target(
    w_study_label_obs,
    w_outcome_obs %>%
      left_join(w_study_key, by = c("study_identifier", "comments")) %>%
      select(-c(study_identifier, comments, title)) %>%
      select(outcome, study, arm, everything()),
    pattern = map(w_outcome_obs),
    iteration = "list"
  ),
  
  # wrangle study-arm labels ------------------------------------------------
  
  tar_target(
    r_condition,
    read_csv("data/conditions-2021-07-12_16:37:56.csv")
  ),
  
  tar_target(
    w_condition,
    r_condition %>%
      select(-condition_no_chronic) %>%
      rename(condition_general = general_grouping,
             condition_iasp = iasp_classification)
  ),
  
  
  tar_target(
    w_study_arm_par,
    w_study_label_study_arm_par  %>%
      mutate(across(everything(), tolower)) %>%
      rename(chronic_condition = condition) %>%
      left_join(w_condition, by = "chronic_condition") %>%
      mutate(
        # label placebo interventions
        intervention = if_else(is.na(intervention) &
                                 type == "placebo",
                               "placebo",
                               intervention),
        # classes
        
        # fix a spelling mistake in classes
        class = if_else(str_detect(class, "tetra"),
                        "tetracyclic (teca)",
                        class),
        
        # one of the placebo classes is labelled "n"
        class = ifelse(class == "n", NA, class)
      )
    
  ),
  
  
  
  # wrangle scales ----------------------------------------------------------
  
  tar_target(r_scales,
             read_rds("data/scales-2021-07-12_16:37:56.rds")),
  
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
  
  
  # observations ------------------------------------------------------------
  
  
  tar_target(
    w_obs_long,
    w_study_label_obs %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = -c(outcome, study, arm),
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
      select(-measure_matches,-covidence_colname) %>%
      mutate(across(everything(), tolower)) %>%
      left_join(m_key, by = "outcome")
    ,
    pattern = map(w_study_label_obs)
  ),
  
  # filters -----------------------------------------------------------------
  
  # currently filtering to neuropathic and scales categorised to 1
  # in order to filter, need study-level things
  
  tar_target(
    w_obs_long_filtered,
    w_obs_long %>%
      left_join(
        w_study_arm_par %>%
          select(study, arm, condition_general, design)
        ,
        by = c("study", "arm")
      ) %>%
      # filter by condition & design
      filter(condition_general == "neuropathic") %>% 
      select(-condition_general, -design)
  ),
  
  # scales ------------------------------------------------------------------
  
  tar_target(w_obs_scale_matches,
             w_obs_long_filtered %>%
               mutate(
                 scale_match = map2(
                   outcome,
                   covidence_desc,
                   .f = function(o, desc) {
                     matches <-
                       w_scales %>%
                       filter(outcome == o) %>%
                       mutate() %>%
                       mutate(
                         aka_det = map_lgl(
                           aka,
                           .f = function(a) {
                             str_detect(desc, a)
                           }
                         ),
                         cat_det = map_lgl(
                           scale_category,
                           .f = function(c) {
                             str_detect(desc, c)
                           }
                         )
                       ) %>%
                       mutate(aka_det =
                                if_else(is.na(aka_det), FALSE, aka_det))  %>%
                       filter(aka_det | cat_det)
                     
                     # filter out the sleep disturbance when rows > 2
                     matches <-
                       if ((nrow(matches) > 1) &
                           str_detect(desc, "sleep_disturbance")) {
                         matches %>%
                           filter(scale_category != "sleep_disturbance")
                       } else
                         matches
                     
                     # deal with vas_0_100
                     matches <-
                       if (str_detect(desc, "vas_0_100")) {
                         matches %>% filter(aka == "vas_0_100")
                       } else
                         matches
                     
                     # anxiety & depression
                     matches <-
                       if (str_starts(desc, "anxiety")) {
                         matches %>%
                           filter(str_detect(outcome_label, "anxiety"))
                       } else if (o == "mood") {
                         matches %>%
                           filter(str_detect(outcome_label, "depression"))
                       } else
                         matches
                     
                     # score rated by patient
                     matches <-
                       if (str_detect(desc,
                                      "depression_score_rated_by_patient")) {
                         matches %>%
                           filter(scale_category != "scale_unknown")
                       } else
                         matches
                     
                     # nrs 0-10
                     # matches <-
                     #   if (nrow(matches > 1) &
                     #       o == "pain_int" &
                     #       str_detect(desc, "nrs")) {
                     #     matches %>%
                     #       filter(str_detect(scale_category, "numerical_rating"))
                     #   } else matches
                     
                     # choose scale category if only one matches the scale cat
                     matches <-
                       if (sum(matches$cat_det) == 1) {
                         matches %>%
                           filter(cat_det)
                       } else if (n_distinct(matches$scale_category) == 1) {
                         matches %>%
                           head(1)
                       } else
                         matches
                     
                     return(matches)
                     
                   }
                 )
               )),
  
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
               cat_n > 1),
  
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
  
  
  
  
  # go wide -----------------------------------------------------------------
  
  
  
  tar_target(
    w_obs_wide,
    w_obs_scales_viable %>%
      pivot_wider(
        id_cols = c(
          outcome,
          study,
          arm,
          covidence_desc,
          scale,
          model_type
        ),
        names_from = measure_type,
        values_from = covidence_value
      ) %>%
      mutate(across(any_of(
        c("mean", "sd", "se", "percent", "median")
      ), as.numeric)) %>%
      mutate(n = as.integer(n))
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
  
  tar_target(
    w_obs_fake_percent,
    w_obs_time %>% 
      mutate(
        percent = NA
      )
  ),
  
  
  # bundle everything together ----------------------------------------------
  
  # get sds
  tar_target(w_obs_calc,
             w_obs_fake_percent %>% 
             # w_obs_time %>%
               mutate(
                 # fix this later
                 r = ifelse(is.na(percent), n, NA),
                 se = if_else(sd > 0 & is.na(se) & n > 0,
                              sd / sqrt(n),
                              se),
                 n = if_else(model_type == "lor" & percent > 0,
                             as.integer(r * 100 / percent),
                             n)
               )),
  
  # todo: target for rows not used
  tar_target(
    w_obs_viable,
    w_obs_calc %>%
      mutate(
        model_viable = case_when(
          model_type == "lor" ~ n > 0 & r > 0,
          model_type == "smd" ~ mean > 0 & se > 0 & n > 0
        )
      ) %>%
      filter(model_viable) %>%
      filter(!is.na(study)) %>%
      select(-model_viable)
  ),
  
  tar_target(w_obs_investigate,
             anti_join(w_obs_calc, w_obs_viable)),
  
  # todo: add filters to observation
  
  # keep this target as the final all-in output dataframe to be shared
  # use this target for EDA
  tar_target(
    w_obs,
    w_obs_viable %>%
      left_join(w_study_arm_par, by = c("study", "arm")) %>%
      # apply filters here for now
      filter(
        timepoint == "post_int",
        str_detect(design, "parallel"),
        # this is a sledgehammer
        type %in% c("placebo", "antidepressant")
      ) %>%
      rename(covidence = covidence_desc) %>%
      select(everything(), covidence) %>%
      # select(-range, -median, -ci, -iqr, -percent, -intervals, -model_type) %>%
      relocate(r, .before = n) %>%
      ungroup()
  ),
  
  # models ------------------------------------------------------------------
  
  
  # inputs ------------------------------------------------------------------
  
  
  tar_target(
    m_dat_outcome,
    w_obs %>%
      filter(outcome == w_outcomes),
    pattern = map(w_outcomes),
    iteration = "list"
  ),
  
  tar_target(
    m_condition_general,
    w_obs %>%
      select(outcome, condition_general) %>%
      distinct()
  ),
  
  tar_target(
    m_dat_condition_general,
    left_join(m_condition_general, w_obs),
    pattern = map(m_condition_general),
    iteration = "list"
  ),
  
  tar_target(
    m_condition_iasp,
    w_obs %>%
      select(outcome, condition_iasp) %>%
      distinct()
  ),
  
  tar_target(
    m_dat_condition_iasp,
    left_join(m_condition_iasp, w_obs),
    pattern = map(m_condition_iasp),
    iteration = "list"
  ),
  
  tar_target(
    m_class,
    w_obs %>%
      mutate(class = if_else(type == "placebo",
                             "placebo",
                             class)) %>%
      select(outcome, class) %>%
      distinct() %>%
      filter(class != "placebo")
  ),
  
  tar_target(
    m_dat_class,
    {
      binding_dat <-
        m_class %>%
        bind_rows(m_class)
      
      binding_dat[2, 2] <- "placebo"
      
      binding_dat %>% left_join(w_obs)
    },
    pattern = map(m_class),
    iteration = "list"
  ),
  
  tar_target(
    m_dat,
    m_dat_outcome %>%
      append(m_dat_condition_general) %>%
      append(m_dat_condition_iasp) %>%
      append(m_dat_class) %>%
      map(
        .f = function(df) {
          df %>%
            group_by(study) %>%
            filter(length(intervention) > 1) %>%
            ungroup()
        }
      )
  ),
  
  
  
  tar_target(
    m_subgroups,
    tibble(
      outcome = w_outcomes,
      subgroup = "all",
      subgroup_value = "all"
    ) %>%
      bind_rows(
        m_condition_general %>%
          rename(subgroup_value = condition_general) %>%
          mutate(subgroup = "condition_general")
      ) %>%
      bind_rows(
        m_condition_iasp %>%
          rename(subgroup_value = condition_iasp) %>%
          mutate(subgroup = "condition_iasp")
      ) %>%
      bind_rows(
        m_class %>%
          filter(class != "placebo") %>%
          rename(subgroup_value = class) %>%
          mutate(subgroup = "class")
      ) %>%
      left_join(m_key, by = "outcome") %>%
      mutate(dat = m_dat,
             nrow = map_int(dat, nrow)) %>%
      filter(nrow > 1)
    
  ),
  
  # nma ---------------------------------------------------------------------
  
  tar_target(
    m_net,
    map2(
      m_subgroups %>% pull(dat),
      m_subgroups %>% pull(model_type),
      hpp_net
    )
    
  ),
  
  tar_target(m_nma_all,
             m_net %>%
               map(safe_nma, trt_effects = "random")),
  
  tar_target(m_beep, {
    m_nma %>% str(1)
    beepr::beep()
  }),
  
  tar_target(
    m_nma_key_all,
    m_subgroups %>%
      mutate(no_fail = m_nma_all %>% map(1) %>% map(length) != 1)
  ),
  
  tar_target(
    m_nma_key,
    m_nma_key_all %>%
      filter(no_fail) %>%
      select(-no_fail)
  ),
  
  tar_target(m_nma,
             m_nma_all[m_nma_key_all %>% pull(no_fail)]),
  
  # eda all outcomes --------------------------------------------------------
  
  
  # outcome and condition ---------------------------------------------------
  
  tar_target(
    e_outcome_condition_counts,
    w_obs %>%
      group_by(outcome, condition_general, type) %>%
      summarise(
        participants = sum(n, na.rm = TRUE),
        studies = n_distinct(study),
        interventions_n = n_distinct(intervention),
        interventions = intervention %>% unique() %>% paste(collapse = "; "),
        classes = class %>% unique() %>% paste(collapse = "; ")
      ) %>%
      ungroup()
  ),
  
  tar_target(
    e_outcome_condition_placebo,
    e_outcome_condition_counts %>%
      filter(type == "placebo") %>%
      rename(
        studies_p = studies,
        interventions_n_p  = interventions_n,
        participants_p = participants
      ) %>%
      select(-interventions,-type,-classes)
  ),
  
  tar_target(
    e_outcome_condition_intervention,
    e_outcome_condition_counts %>%
      filter(type == "antidepressant") %>%
      rename(
        studies_i = studies,
        interventions_n_i  = interventions_n,
        participants_i = participants
      ) %>%
      select(-type)
  ),
  
  tar_target(
    e_outcome_condition_all_types,
    w_obs %>%
      group_by(outcome, condition_general) %>%
      summarise(
        participants_t = sum(n, na.rm = TRUE),
        studies_t = n_distinct(study),
        interventions_n_t = n_distinct(intervention),
      )
  ),
  
  tar_target(
    e_outcome_condition,
    left_join(
      e_outcome_condition_placebo,
      e_outcome_condition_intervention,
      by = c("outcome", "condition_general")
    ) %>%
      left_join(
        e_outcome_condition_all_types,
        by = c("outcome", "condition_general")
      )
  ),
  
  tar_target(
    e_outcome_condition_tab,
    e_outcome_condition %>%
      ungroup() %>%
      gt(groupname_col = "outcome",
         rowname_col = "condition_general") %>%
      hpp_tab() %>%
      summary_rows(
        columns = c(
          contains("participants"),
          contains("studies"),
          contains("interventions_n")
        ),
        fns = list(Total = ~ sum(., na.rm = TRUE)),
        groups = TRUE,
        formatter = fmt_number,
        decimals = 0
      ) %>%
      tab_spanner(label = "placebo",
                  columns = contains("_p")) %>%
      tab_spanner(label = "intervention",
                  columns = contains("_i")) %>%
      tab_spanner(label = "total",
                  columns = contains("_t")) %>%
      cols_label(
        participants_p = "participants",
        studies_p = "studies",
        interventions_n_p = "interventions",
        participants_i = "participants",
        studies_i = "studies",
        interventions_n_i = "interventions",
        participants_t = "participants",
        studies_t = "studies",
        interventions_n_t = "interventions"
      ) %>%
      tab_header(
        "Outcomes and conditions",
        "Number of participants, studies, and interventions in for each
        condition in each outcome"
      ) %>%
      tab_style(style = list(cell_fill(hpp_pal(
        1
      ))),
      locations = cells_summary()) %>%
      tab_style(
        style = cell_text(size = "x-small"),
        locations =
          cells_column_labels(columns = c(
            contains("participants"),
            contains("studies"),
            contains("interventions_n")
          ))
      )
  ),
  
  # shiny -------------------------------------------------------------------
  
  # for some reason the structure of the all ins are different
  
  tar_target(shiny_nma,
             write_rds(m_nma, "hppshiny/nma-2.rds")),
  
  tar_target(
    shiny_key ,
    write_rds(m_nma_key %>% select(-dat), "hppshiny/key-2.rds")
  ),
  
  
  
  # export data -------------------------------------------------------------
  tar_target(
    export_dat_files,
    m_nma_key %>%
      select(-nrow,-model_type) %>%
      mutate(
        file_name = glue("exports/hppdat/{outcome}-{subgroup}-{subgroup_value}.csv")
      )
  ),
  
  tar_target(
    export_data,
    map2(
      export_dat_files %>% pull(dat),
      export_dat_files %>% pull(file_name),
      write_csv
    )
  ),
  
  
  # null --------------------------------------------------------------------
  
  
  
  
  NULL
)
