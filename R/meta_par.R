#' Add meta par
#'
#'@param obs From [assign_timepoints].
#'
#'@export

meta_par <- function(obs, dat) {
  dat %>%
    janitor::clean_names() %>% 
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