#' Final filters
#'
#'
#' @param obs Observations

wrangle_netdat <- function(obs) {
  wrangle_all <-
  obs %>%
    dplyr::filter(
      timepoint == "post_int",
      # can't do studies where there is no mean or se
      # !is.na(any_of(c("mean", "se", "percent", "n"))),
      # remove the crossover studies
      group != "Crossover"
    )

  # filter where I don't have the effect/variance of effect.
  if ("mean" %in% colnames(obs)) {
    wrangle_all %>%
      filter(!is.na(mean), !is.na(se))
  } else if ("percent" %in% colnames(obs)) {
    wrangle_all %>%
      filter(!is.na(percent), !is.na(n))

  }

}

#' Wrangle netdat
#
# @param pain_dat from obs_pain_wrangled.
# wrangle_netdat_pain <- function(pain_dat) {
#     pain_dat %>%
#     filter(timepoint == "post_int",
#            !is.na(mean), !is.na(se),
#            # remove the crossover studies
#            group != "Crossover",
#            # single-arm studies are not supported
#            study_identifier != "Agger 2017"
#     ) %>%
#     rename(
#       scale = measure_scale,
#       study = study_identifier,
#       intervention = intervention_drug,
#       arm = intervention
#     )
#
# }
