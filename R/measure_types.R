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
    mutate(
      measure_desc = str_extract(column_header, "[\\w+|%]$")) %>% 
    # dplyr::mutate(
    #   measure_matches =
    #     purrr::map(
    #       column_header,
    #       stringr::str_match,
    #       "(.*)_([a-z]+)(?:_\\d)*$"
    #     ),
    #   measure_desc = purrr::map_chr(measure_matches, purrr::pluck, 2),
    #   measure = purrr::map_chr(measure_matches, purrr::pluck, 3)
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

