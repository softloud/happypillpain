#' Preliminary cleaning before any analysis
#'
#' Cleans names and creates a study/arm identifier.
#'
#' @export

preliminary_scrub <- function(dat) {
  cleaned_names <-
    dat %>%
    janitor::clean_names()

  # study id tags
  study_tags <-
  cleaned_names %>%
    select(study_identifier, comments) %>%
    distinct() %>%
    group_by(study_identifier) %>%
    mutate(
      tag_n = 1:n(),
      max_tag = max(tag_n),
      study_tag = if_else(
        max_tag > 1L,
        glue("{study_identifier} | {tag_n}"),
        study_identifier
      )
    ) %>%
    select(study_identifier, comments, study_tag)

  cleaned_names %>%
    left_join(study_tags, by = c("study_identifier", "comments")) %>%
    dplyr::mutate(
      # change study identifier
      study_identifier = study_tag,
      # arm id
      study_arm_id = stringr::str_c(intervention, study_identifier, sep = " ||| "),
      intervention_drug = purrr::map_chr(
        intervention,
        .f = function(x) {
          treatment <- tolower(x)

          if (stringr::str_detect(treatment, "and|\\+") &
              !stringr::str_detect(treatment, "[and|\\+]\\s[^a-z]")) {
            combination <-
              stringr::str_match(treatment, "(\\w+).+[\\+|and]\\s(\\w+)")
            paste0(combination[-1], collapse = " + ")
          } else {
            stringr::str_extract(treatment, "\\w+")
          }
        }
      )
    ) %>%
    dplyr::select(study_arm_id, everything()) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
}
