#' Extract based on columns
#' 
#' @param outcome pain or mood, etc.
#' @param dat hpp_dat in raw form
#' @param outcome_cols list of columns

get_outcome_cols <- function(outcome, dat, outcome_cols) {
  
  relevant_cols <- outcome_cols %>% pluck(outcome)
  
  dat %>% 
    select(study_arm_id,  
           all_of(relevant_cols)) %>%
    janitor::clean_names() %>% 
    tidyr::pivot_longer(
      cols = -c(study_arm_id, 
                intervention, 
                intervention_type,
                intervention_name),
      names_to = "column_header",
      values_to = "obs",
      values_drop_na = TRUE
    )

}
