#' Extract observations
#'
#' Note that at the moment everything is included, but will need to update
#' the code for exclusions and combinations, as well.
#'
#' Currently this is designed to follow from [preliminary_scrub] and output to
#' the [net_outcome] functions.
#'
#' This does all the data handling that will happen on every outcome:
#' the objective is to perform the manipulations that are required for every
#' measure. Based on keywords, identify outcomes and massage into a long-format
#' meta-analysis dataset wherein each row signifies the reported statistics for
#' one arm of a study.
#'
#' Best not to be too ambitious.
#' If the original dataset is also given as an argument to the next function,
#' any other covariates, etc. can be joined there. Only add to this function if
#' it is generalisable for all functions.
#'
#' @param outcome Category from gsheet of outcome [outcome_keywords].
#' @param dat [hpp_dat] data.
#' @param raw_names Names of the raw hpp dataset before [preliminary_scrub].
#' @param keywords From data-raw/keywords.csv, created in data-raw/create-keywords.R.
#' @param measures_troubleshoot For troubleshooting measures.
#'
#' @export

extract_obs <-
  function(outcome,
           dat,
           keywords,
           measures_troubleshoot = FALSE) {
    
    
    keywords_for_outcome <-
      outcome_keywords(outcome, keywords) %>%
      dplyr::filter(inclusion_criterion == "include") %>%
      # this is hacky as it assumes we'll include all columns that match
      # and there is no combination or exclusion
      dplyr::pull(keyword) %>% 
      paste0(collapse = "|")

    keywords_to_exclude <-
      outcome_keywords(outcome, keywords) %>%
      dplyr::filter(inclusion_criterion == "exclude") %>%
      dplyr::pull(keyword) %>% 
      paste0(collapse = "|")
    
    exclude_column <- function(col_name) {
      if (keywords_to_exclude == "") {
        return(FALSE)
      } else {
        str_detect(col_name, keywords_to_exclude)
      }
    }

    relevant_columns <- 
      tibble(
        col = dat %>% names(),
        include = str_detect(col, keywords_for_outcome),
        exclude = map_lgl(col, exclude_column)
      ) %>% 
      filter(
        include & !exclude
      ) %>% 
      pull(col)
    
    assertthat::assert_that(length(relevant_columns) > 0, 
                            msg = glue("Did not extract any columns based on keywords for outcome: {outcome}"))
    
    # get relevant columns and pivot longer
    dat %>%
    select(study_arm_id, relevant_columns) %>% 
            tidyr::pivot_longer(
        cols = -study_arm_id,
        names_to = "column_header",
        values_to = "obs",
        values_drop_na = TRUE
      )

  }



#' Check function for multiple measures
#'
#' This is for where there are multiple measures, otherwise,
#' I suspect, [measures_wide] won't run.
#'
#' I also need to tag the measure, so, this can function as that as well
#' for now.
#'
#' @export

measure_types <- function(dat) {
  mtype_dat <-
  dat %>%
    dplyr::mutate(
      measure_matches =
        purrr::map(
          column_header,
          stringr::str_match,
          "(.*)_([a-z]+)(?:_\\d)*$"
        ),
      measure_desc = purrr::map_chr(measure_matches, purrr::pluck, 2),
      measure = purrr::map_chr(measure_matches, purrr::pluck, 3)
    ) %>%
    dplyr::select(-column_header, -measure_matches) %>%
    # set up measure types
    dplyr::mutate(
      measure_scale =
        dplyr::case_when(
          stringr::str_detect(measure_desc, "_vas_") ~ "vas",
          stringr::str_detect(measure_desc, "_bpi_") ~ "bpi",
          stringr::str_detect(measure_desc, "_scl_") ~ "scl",
          stringr::str_detect(measure_desc, "_rps_") ~ "rps",
          stringr::str_detect(measure_desc, "_nrs_") ~ "nrs",
          stringr::str_detect(measure_desc, "_bocf_") ~ "bocf",
          stringr::str_detect(measure_desc, "_mcgill_|_mpq_") ~ "mpq",
          TRUE ~ "not matched"
        )
    )

  # assertthat::assert_that(
  #   mtype_dat %>%
  #     dp
  #
  #   msg = "some arms have more than one mean"
  # )

}


#' Identify measurements
#'
#' Ignoring counters at the end, assuming measure is at the end before possible
#' duplication tag, count the number and type (percent, n, limits, mean, sd,
#' etc.) from the end.
#'
#'
#' Assumes stringr::structure of the stringr::string:
#' 1. description
#' 2. measure
#' 3. possible duplication tag
#'
#' Pivot wide by measure.
#'
#'
#' @param obs_long Long-form observations for an outcome.
#' @param count_obs Troubleshooting toggle that counts how many duplicates. Call
#' [extract_obs] with `measures_troubleshoot` to get the long-form expected.
#'
#' @export

measures_wide <- function(obs_long, count_obs = FALSE) {
  # not working, see below
  # v_fn <- if (isTRUE(count_obs)) NULL else length



  # need to split so I can troubleshoot the
  obs_long %>%
    tidyr::pivot_wider(
      id_cols = c(study_arm_id, measure_desc, measure_scale),
      names_from = measure,
      values_from = obs,
      values_fill = NA
      # troubleshooting
      # values_fn = v_fn # this code isn't working
    )
  # %>%
  #   dplyr::mutate(dplyr::across(matches('mean|sd|se|median|percent'), as.numeric))
  # %>%
  #   dplyr::mutate(n = as.integer(n))
}

#' Roughly categorise timepoints
#'
#' This may need to be finessed for each extraction.
#'
#' Expects dataframe produced by [measures].
#'
#' Only does post intervention atm.
#'
#' @export

assign_timepoints <- function(obs_wide) {
  obs_wide %>%
    dplyr::mutate(timepoint = dplyr::case_when(
      stringr::str_detect(measure_desc, "post_intervention") ~ "post_int",
      TRUE ~ "not_post_int"
    ))
}

#' Get keywords
#'
#' Extract dataframe from [googlesheet](https://docs.google.com/spreadsheets/d/1vUY0gJ0bl-huVO-QIy7hjXwNRe87rPIt0cO4fLZobEQ/edit#gid=0)
#'
#' @param outcome Specify which outcome from outcome column of keywords.
#' @param keywords Keywords dataset.
#'
#' @export

outcome_keywords <- function(outcome, keywords) {
  keywords %>%
    dplyr::filter(outcome == !!outcome) %>% 
    mutate(across(everything(), tolower)) %>% 
    mutate(across(everything(), str_replace, "\\s", "_"))
}



#' Convert measures to numeric
#'
#' This is failing so often it'd be useful to have a debugging algorithm
#' for it.
#'
#' @param outcome_obs As created by [].
#'
#' @export

measures_numeric <- function(outcome_obs) {
  obs_numeric <-
    outcome_obs %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c("mean", "se", "sd", "median", "mean", "percent")
    ),
    as.numeric)) %>%
    dplyr::mutate(n = as.integer(n))

  se_sd_fixed <-
    # fix se and sd
    if ("sd" %in% colnames(obs_numeric)) {
      obs_numeric %>%
        dplyr::mutate(se = dplyr::if_else(is.na(se), sd / sqrt(n), se))
    } else
      obs_numeric
}

#' Add meta par
#'
#'@param obs From [assign_timepoints].
#'
#'@export

meta_par <- function(obs, dat) {
  dat %>%
    dplyr::select(
      study_arm_id,
      study_identifier,
      comments,
      intervention,
      intervention_type,
      intervention_class,
      intervention_drug,
      design,
      group,
      dplyr::starts_with("main_aim"),
      dplyr::starts_with("chronic_pain_condition")
    ) %>%
    dplyr::right_join(obs, by = "study_arm_id")

}
