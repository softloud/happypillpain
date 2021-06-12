#' Return all matching scales for outcome
#'
#' Write for scales to exist in the environment.
#'
#' @param desc  measure_desc column in wo_scale
#' @param outcome outcome: pain, mood, etc.


scale_match <- function(desc, outcome, scales_df) {
  matches <-
    scales_df %>%
    dplyr::filter(outcome == !!outcome) %>%
    select(scale_category, scale) %>%
    mutate(
      scale_detected = str_detect(desc, scale)) %>%
    filter(scale_detected) %>%
    select(-scale_detected)
  
  # for dealing with 0-10 and 0-100
  if (any(str_detect(matches$scale_category, "0_100"))) {
    matches <-
      # remove the 0-10 from matches
      matches %>%
      filter(scale_category != "0_10_scale")
  }
  
  # for dealing with 0 100 4 weeks
  if (all(c("0_100_scale", "0_4_scale") %in% matches$scale_category)) {
    matches <-
      matches %>%
      filter(scale_category != "0_4_scale")
  }
  
  if (all(c("0_10_scale", "0_4_scale") %in% matches$scale_category)) {
    matches <-
      matches %>%
      filter(scale_category != "0_4_scale")
  }
  
  if (length(matches$scale_category) == 2 &
      any(str_detect(matches$scale_category, "0_10"))) {
    matches <-
      matches %>%
      filter(!str_detect(scale_category, "0_10"))
  }
  
  if (str_detect(desc, "npsi")) {
    matches <- 
      matches %>% 
      filter(scale != "nps")
  }
  
  if (str_detect(desc, "pain_24_hour_average_pain_score_bpi_sf_item_5")) {
    matches <- 
      matches %>% 
      filter(str_detect(scale_category, "0_10"))
  }
  
  n_matches <-
    matches %>%
    pull(scale_category) %>%
    unique() %>%
    length()

  if (n_matches == 1) {
    matches %>% pull(scale_category) %>% unique()
  } else if (outcome == "adverse") {
    "count (binomial) data"
  } else if (n_matches == 0) {
    "no matches"
  } else {
    "multiple matches"
  }
  
  
}