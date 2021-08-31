#' Using study identifier and title_covidence to create a study id
#' 
#' Fiddly and I use it more than once.

study_id <- function(df) {
  df %>%
    select(study_covidence, title_covidence) %>% 
    distinct() %>% 
    group_by(study_covidence) %>% 
    mutate(
      tag = 1:n(),
      max_tag = max(tag),
      study = if_else(
        max_tag > 1,
        glue("{study_covidence} : {tag}"),
        study_covidence
      )
    ) %>% 
    select(-tag, -max_tag) %>% 
    right_join(df, by = c("study_covidence", "title_covidence")) %>% 
    ungroup() 
}

