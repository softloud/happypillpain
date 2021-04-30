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
      stringr::str_detect(measure_desc, "post-intervention|endpoint") ~ "post_int",
      str_detect(measure_desc, "mid-intervention|within trial") ~ "mid_int",
      str_detect(measure_desc, "baseline") ~ "baseline",
      TRUE ~ "unmatched"
    ))
}