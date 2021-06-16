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
  "scale_match",
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
    ) %>%
      gt() %>%
      hpp_tab(vertical_divider = "arm") %>%
      tab_header("Study-level information drawn from Covidence")
    
  ),
  
  tar_target(
    p_obs,
    tibble(
      outcome = "mood, pain, etc.",
      study = "unique study identifier",
      arm = "unique arm identifier",
      obs = "columns with mean, sd, counts, sample size, etc.",
      obs_info = "information extracted from column headers:
      timepoint, scale, etc."
    ) %>%
      gt() %>%
      hpp_tab(vertical_divider = "arm") %>%
      tab_header("Outcome-level observational from data extracted by Hollie")
    
  ),
  
  
  
  
  # variables ---------------------------------------------------------------
  
  # table with covidence column names and preferred column names
  tar_target(
    r_variables,
    read_csv("data/variables-2021-06-12\ 23:01:37.csv")
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
               read_csv("data/review_91309_extracted_data_csv_20210614040732.csv")
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
               read_csv("data/outcome-adverse-2021-05-19.csv")
             ))),
  
  tar_target(r_h_outcome_mood,
             suppressWarnings(suppressMessages(
               read_csv("data/outcome-mood-2021-05-19.csv")
             ))),
  
  tar_target(r_h_outcome_pain,
             suppressWarnings(suppressMessages(
               read_csv("data/outcome-pain-2021-05-19.csv")
             ))),
  
  tar_target(
    r_h_outcome_obs,
    list(
      mood_depression = r_h_outcome_mood,
      pain = r_h_outcome_pain,
      adverse = r_h_outcome_adverse
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
      select(-c(intervention_name, intervention_type)) %>%
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
      select(-study_identifier, -comments) %>%
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
  
  tar_target(r_condition,
             read_csv("data/condition-2021-06-12.csv")),
  
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
        intervention = if_else(is.na(intervention) & type == "placebo",
                               "placebo",
                               intervention)
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
        measure_matches = map(covidence_colname, str_match, "(.+)_([a-z]+)[_\\d]*$"),
        measure_type = map_chr(measure_matches, 3),
        measure_desc = map_chr(measure_matches, 2)
      ) %>%
      select(-measure_matches, -covidence_colname) %>%
      mutate(across(everything(), tolower))
    ,
    pattern = map(w_study_label_obs)
  ),
  
  tar_target(
    w_obs_wide,
    w_obs_long %>%
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
  
  
  # timepoints --------------------------------------------------------------
  
  tar_target(
    w_obs_time,
    w_obs_wide %>%
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
  
  
  # scales ------------------------------------------------------------------
  
  # import scales data
  
  tar_target(r_scales,
             read_csv("data/scales-2021-06-12.csv")),
  
  tar_target(
    w_scales,
    r_scales %>%
      clean_names() %>%
      mutate(
        outcome =
          if_else(
            str_detect(outcome, "Mood") & str_detect(outcome, "depression"),
            "mood_depression",
            outcome
          )
      ) %>%
      mutate(scale_raw = scale) %>%
      mutate(across(everything(), tolower)) %>%
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
          str_replace_all("[\\s-]", "_")
      ) %>%
      rename(scale_category = scale)
  ),
  
  tar_target(
    w_obs_scale_matches,
    w_obs_time %>%
      mutate(
        matched_scales =
          map2(measure_desc, outcome, scale_match, scales_df = w_scales),
        match_length = map_int(matched_scales, length)
      )
  ),
  
  
  tar_target(
    w_obs_scales,
    w_obs_scale_matches %>%
      filter(match_length == 1) %>%
      mutate(scale = as.character(matched_scales)) %>%
      select(-matched_scales,-match_length)
  ),
  
  # scales are broken!!! everything is unmatched
  # short-term fix
  # filter out studies with direction higher
  tar_target(
    w_obs_higher_to_filter,
    w_scales %>% filter(direction_of_improvement == "higher",
                        # we don't have sleep right now
                        outcome != "sleep")
  ),
  
  tar_target(
    w_obs_scale_lower,
    w_obs_time %>%
      filter(
        !str_detect(measure_desc, "nottingham_health_profile"),
        !str_detect(measure_desc, "mental_component") &
          !str_detect(measure_desc, "sf_36"),
        !str_detect(measure_desc, "mental_health_subscale") &
          !str_detect(measure_desc, "sf_36"),
        !str_detect(measure_desc, "bodily_pain_subscale|body_pain_subscale") &
          !str_detect(measure_desc, "sf_36"),
        
      )
  ),
  
  
  # bundle everything together ----------------------------------------------
  tar_target(m_model_key,
             tibble(
               outcome = w_outcomes,
               model_type = case_when(outcome == "adverse" ~ "binomial",
                                      TRUE ~ "smd")
             )),
  
  # get sds
  tar_target(
    w_obs_calc,
    w_obs_scale_lower %>%
      left_join(m_model_key, by = "outcome") %>%
      mutate(
        r = n,
        se = if_else(sd > 0 & is.na(se) & n > 0,
                     sd / sqrt(n),
                     se),
        n = if_else(model_type == "binomial" & percent > 0,
                    as.integer(r * 100 / percent),
                    n)
      )
  ),
  
  # todo: target for rows not used
  tar_target(
    w_obs_viable,
    w_obs_calc %>%
      mutate(
        model_viable = case_when(
          model_type == "binomial" ~ n > 0 & r > 0,
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
      rename(covidence = measure_desc) %>% 
      select(everything(), covidence) %>%
      select(-range,-median,-ci,-iqr,-percent,-intervals,-model_type) %>%
      relocate(r, .before = n) %>%
      ungroup()
  ),
  
  # models ------------------------------------------------------------------
  
  
  # inputs ------------------------------------------------------------------
 
  tar_target(m_key,
             tibble(
               outcome = w_outcomes
               ) %>% 
                 mutate(
                   model_type = case_when(
                     outcome == "adverse" ~ "binomial",
                     TRUE ~ "smd"
                       
                 )
             )
             ),
  
  tar_target(
    m_dat_outcome,
    w_obs %>% 
      filter(outcome == w_outcomes),
    pattern = map(w_outcomes),
    iteration = "list"
  ),
  
  tar_target(m_condition_general,
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
 
 tar_target(m_condition_iasp,
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
  
 # tar_target(m_class,
 #            w_obs %>% 
 #              mutate(
 #                class = if_else(
 #                  type == "placebo",
 #                  "placebo",
 #                  class
 #                )
 #              ) %>% 
 #              select(outcome, class) %>% 
 #              distinct()
 # ), 
 
 # tar_target(m_classes,
 #            m_class %>% filter(class != "placebo") %>% pull(class)
 #            ),
 
 # tar_target(
 #   m_dat_class,
 #   m_class %>% 
 #    filter(class %in% c(m_classes, "placebo")) %>% 
 #   right_join(w_obs),
 #   pattern = map(m_classes),
 #   iteration = "list"
 # ),
 
  tar_target(
    m_dat,
    m_dat_outcome %>% 
      append(m_dat_condition_general) %>% 
      append(m_dat_condition_iasp) %>% 
      # append(m_dat_class) %>% 
      map(.f = function(df){
        df %>% 
          group_by(study) %>%
          filter(length(intervention) > 1) %>%
          ungroup() 
      })
  ),
 
 
  
  tar_target(m_subgroups,
               tibble(
                 outcome = w_outcomes,
                 subgroup = "all",
                 subgroup_value = "all"
               ) %>% 
               bind_rows(m_condition_general %>% 
                           rename(subgroup_value = condition_general) %>% 
                           mutate(subgroup = "condition_general")) %>%
               bind_rows(m_condition_iasp %>% 
                           rename(subgroup_value = condition_iasp) %>% 
                           mutate(subgroup = "condition_iasp")) %>%
               # bind_rows(m_class %>%
               #             filter(class != "placebo") %>% 
               #             rename(subgroup_value = class) %>% 
               #             mutate(subgroup = "class")) %>%
               left_join(m_key, by = "outcome") %>% 
               mutate(
                 dat = m_dat,
                 nrow = map_int(dat, nrow)
               ) %>% 
               filter(nrow > 1)

  ),
  
# nma ---------------------------------------------------------------------

 tar_target(
   m_net,
     map2(
       m_subgroups %>% pull(dat),
       m_subgroups %>% pull(model_type),
       hpp_net)
   
 ),

 tar_target(m_nma_all,
            m_net %>% 
              map(safe_nma, trt_effects = "random")
            ),

tar_target(m_beep, {
m_nma %>% str(1)
  beepr::beep()
}
           ),
  
tar_target(
  m_nma_key_all,
  m_subgroups %>% 
    select(-dat) %>% 
    mutate(no_fail = m_nma_all %>% map(1) %>% map(length) != 1)
),

tar_target(
  m_nma_key,
  m_nma_key_all %>% 
    filter(no_fail) %>% 
    select(-no_fail)
),

tar_target(
  m_nma,
  m_nma_all[m_nma_key_all %>% pull(no_fail)]
),

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
      select(-interventions, -type, -classes)
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

tar_target(
  shiny_nma,
  write_rds(m_nma, "hppshiny/nma.rds")
),
  
tar_target(shiny_key,
           write_rds(m_nma_key, "hppshiny/key.rds")),


# null --------------------------------------------------------------------



  
  NULL
)
