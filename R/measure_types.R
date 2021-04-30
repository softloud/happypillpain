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
    dat %>%
    mutate(
      column_header = tolower(column_header),
      matches = map(column_header, str_match, "(.+)\\s([a-z %]+)_*\\d*$") ,
       measure = map_chr(matches, 3),
      measure = if_else(measure == "%", "percent", measure),
      measure_desc = map_chr(matches, 2)
      ) %>% select(-matches, -column_header)
  # %>% 
  #   # set up measure types
  #   dplyr::mutate(
  #     measure_scale =
  #       dplyr::case_when(
  #         stringr::str_detect(column_header, "_vas_") ~ "vas",
  #         stringr::str_detect(column_header, "_bpi_") ~ "bpi",
  #         stringr::str_detect(column_header, "_scl_") ~ "scl",
  #         stringr::str_detect(column_header, "_rps_") ~ "rps",
  #         stringr::str_detect(column_header, "_nrs_") ~ "nrs",
  #         stringr::str_detect(column_header, "_bocf_") ~ "bocf",
  #         stringr::str_detect(column_header, "_mcgill_|_mpq_") ~ "mpq",
  #         TRUE ~ "not matched"
  #       )
  #   )
  
 
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
      id_cols = c(study_arm_id, measure_desc),
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

