#' Using study identifier and comments to create a study id
#' 
#' Fiddly and I use it more than once.

study_id <- function(df) {
  df %>%
    select(study_identifier, title) %>% 
    distinct() %>% 
    group_by(study_identifier) %>% 
    mutate(
      tag = 1:n(),
      max_tag = max(tag),
      study = if_else(
        max_tag > 1,
        glue("{study_identifier} : {tag}"),
        study_identifier
      )
    ) %>% 
    select(-tag, -max_tag) %>% 
    right_join(df, by = c("study_identifier", "title")) %>% 
    select(-study_identifier) %>% 
    ungroup() 
}

