#' Counts variables of interest
#'
#' How many observations do we have for each category?
#'
#' How many studies?
#'
#' @param netdat A netdat object (see visnetwork).
#'
#' @returns Dataset of counts
#'
#' @export

count_categories <- function(netdat) {
  netdat %>%
    dplyr::count(intervention_type, intervention, chronic_pain_condition_s, timepoint)
}

#' Basic summary count table
#'
#' Generic output that will be the same for all studies at the first review.
#'
#' @param count_gt_dat Data from [count_categories].
#'
#' @return gt plot.
#'
#' @export

count_gt <- function(count_gt_dat) {
netdat %>%
    dplyr::group_by(intervention_drug, intervention_type) %>%
    dplyr::summarise(
      studies = dplyr::n_distinct(study_identifier),
      dosages = dplyr::n_distinct(intervention),
      groups = dplyr::n_distinct(group)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(intervention_type) %>%
    gt::gt()  %>%
    gt::summary_rows(
      groups = TRUE,
      columns = gt::vars(studies, dosages, groups),
      fns = list(total = ~sum(.)),
      formatter = fmt_number,
      use_seps = FALSE
    )
}
