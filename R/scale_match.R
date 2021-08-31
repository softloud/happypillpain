#' Match scales to categories
#' 
#' @param o Outcome
#' @param desc Covidence description
#' @param m Model type

scale_match <- function(o, desc, m, scale_df) {
  matches <-
    scale_df %>%
    filter(outcome == o) %>%
    mutate() %>%
    mutate(
      aka_det = map_lgl(
        aka,
        .f = function(a) {
          str_detect(desc, a)
        }
      ),
      cat_det = map_lgl(
        scale_category,
        .f = function(c) {
          str_detect(desc, c)
        }
      )
    ) %>%
    mutate(aka_det =
             if_else(is.na(aka_det), FALSE, aka_det))  %>%
    filter(aka_det | cat_det)
  
  message("filter out the sleep disturbance when rows > 2")
  matches <-
    if ((nrow(matches) > 1) &
        str_detect(desc, "sleep_disturbance")) {
      matches %>%
        filter(scale_category != "sleep_disturbance")
    } else
      matches
  
  message("0_100")
  matches <-
    if (nrow(matches) > 1 &
        str_detect(desc,
                   # "_0_100|0_100_|100_mm_vas|100mm_vas")
                   "100"
        )) {
      matches %>% filter(str_detect(scale_category, "100"))
    } else
      matches
  
  message("anxiety & depression")
  matches <-
    if (str_starts(desc, "anxiety")) {
      matches %>%
        filter(str_detect(outcome_label, "anxiety"))
    } else if (o == "mood") {
      matches %>%
        filter(str_detect(outcome_label, "depression"))
    } else
      matches
  
  message("score rated by patient")
  matches <-
    if (str_detect(desc,
                   "depression_score_rated_by_patient")) {
      matches %>%
        filter(scale_category != "scale_unknown")
    } else
      matches
  
  message("nrs 0-10")
  matches <-
    if (nrow(matches) > 1 &
        o == "pain_int" &
        str_detect(desc, "nrs|numerical_rating_scale")) {
      matches %>%
        filter(str_detect(scale_category, "numerical_rating"))
    } else
      matches
  
  message("mcgill short form")
  matches <-
    if (nrow(matches) > 1 &
        str_detect(desc, "short_form_mc_gill")) {
      matches %>%
        filter(str_detect(scale_category, "short_form"))
    } else
      matches
  
  message("present pain intensity")
  matches <-
    if (nrow(matches) > 1 &
        str_detect(desc, "present_pain_intensity")) {
      matches %>%
        filter(str_detect(scale_category, "present_pain_intensity"))
    } else
      matches
  
  message("brief pain short form item 5")
  matches <-
    if (nrow(matches) > 1 &
        str_detect(desc, "bpi_sf_item_5")) {
      matches %>%
        filter(str_detect(
          scale_category,
          "brief_pain_short_form_inventory_item_5"
        ))
    } else
      matches
  
  message("eq vas")
  matches <-
    if (nrow(matches) > 1 &
        str_detect(desc, "eq_5d_vas")) {
      matches %>%
        filter(str_detect(scale_category, "vas"))
    } else
      matches
  
  message("fhaq")
  matches <-
    if (nrow(matches) > 1 & str_detect(desc, "fhaq|f_haq")) {
      matches %>%
        filter(str_detect(aka, "fhaq|f_haq"))
    } else matches
  
  message("differentiate the unspecified")
  matches <-
    if (nrow(matches) > 1 &
        str_detect(desc,
                   "\\d+.*likert|likert.*\\d+|\\d+.*vas|vas.*\\d+|\\d+.*nrs|nrs.*\\d+")) {
      matches %>%
        filter(!str_detect(scale_category, "unspecified"))
    } else if (nrow(matches) > 1 &
               str_detect(desc, "liker|vas|nrs") &
               !str_detect(desc,
                           "\\d+.*likert|likert.*\\d+|\\d+.*vas|vas.*\\d+|\\d+.*nrs|nrs.*\\d+")) {
      matches %>%
        filter(str_detect(scale_category, "unspecified"))
      
    } else
      matches
  
  message("choose scale category if only one matches the scale cat")
  matches <-
    if (sum(matches$cat_det) == 1) {
      matches %>%
        filter(cat_det)
    } else if (n_distinct(matches$scale_category) == 1) {
      matches %>%
        head(1)
    } else
      matches
  
  matches <-
    if (m == "lor") {
      tibble(scale_category = "count")
    } else
      matches
  
  return(matches)
  
}