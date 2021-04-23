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
#' @param outcome_cols String of outcomes to select.
#' @param measures_troubleshoot For troubleshooting measures.
#'
#' @export

extract_obs <-
  function(outcome,
           dat,
           # keywords,
           cnames,
           measures_troubleshoot = FALSE) {
    
    
    # keywords_for_outcome <-
    #   outcome_keywords(outcome, keywords) %>%
    #   dplyr::filter(inclusion_criterion == "include") %>%
    #   # this is hacky as it assumes we'll include all columns that match
    #   # and there is no combination or exclusion
    #   dplyr::pull(keyword) %>% 
    #   paste0(collapse = "|")
    # 
    # keywords_to_exclude <-
    #   outcome_keywords(outcome, keywords) %>%
    #   dplyr::filter(inclusion_criterion == "exclude") %>%
    #   dplyr::pull(keyword) %>% 
    #   paste0(collapse = "|")
    # 
    # exclude_column <- function(col_name) {
    #   if (keywords_to_exclude == "") {
    #     return(FALSE)
    #   } else {
    #     str_detect(col_name, keywords_to_exclude)
    #   }
    # }
    # 
    # relevant_columns <- 
    #   tibble(
    #     col = dat %>% names(),
    #     include = str_detect(col, keywords_for_outcome),
    #     exclude = map_lgl(col, exclude_column)
    #   ) %>% 
    #   filter(
    #     include & !exclude
    #   ) %>% 
    #   pull(col)
    
    dat %>% 
      select()
    
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




