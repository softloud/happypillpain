#' Get wide-form pairwise comparisons
#'
#' @param dat long-format outcome data
#' @param trt one treatment
#' 
#' @export

pairwise_wide <- function(dat, trt) {
 long_obs <-
    dat %>%
    # filter by treatment, in addition to outcome
    filter(intervention_drug == {
      {
        trt
      }
    } | intervention_drug == "placebo") %>%
    # all of this is dealing with duplicate placebos
    
    # tag counts
    group_by(study_identifier) %>%
    mutate(placebo_duplicate = sum(intervention_drug == "placebo") > 1) %>%
    # filter out studies that only have placebos
    filter(sum(intervention_drug != "placebo") != 0) %>%
    group_by(study_identifier, intervention_drug) %>%
    mutate(trt_n = 1:n()) %>%
    mutate(study_identifier = if_else(
      placebo_duplicate,
      str_c(study_identifier, as.character(trt_n), sep = " | "),
      as.character(study_identifier)
    )) %>% 
    select(-placebo_duplicate,-trt_n) %>%
    ungroup()
  
  list(
    placebo = long_obs %>% filter(str_detect(trt, "placebo")),
    trt = long_obs %>% filter(!str_detect(trt, "placebo"))
  )
  
  # placebo
  placebo_obs <-
    long_obs %>%
    filter(str_detect(trt, "placebo"))
  # %>%
  #   rename(
  #     placebo_effect = effect,
  #     placebo_se = effect_se,
  #     placebo_n = effect_n
  #   ) %>%
  #   select(-trt)

  trt_obs <-
    long_obs %>%
    filter(!str_detect(trt, "placebo"))
  # %>%
  #   rename(trt_effect = effect,
  #          trt_se = effect_se,
  #          trt_n = effect_n) %>%
  #   select(-trt)

  wide_pairwise <-
    left_join(trt_obs, placebo_obs, by = c("study_identifier"))
  
}


#' Pairwise meta-analysis

rmas_smd <- function(dat) {
 # trt <- 
    
    
}