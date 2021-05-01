#' Convert a column to clean-names equivalent
#' 

clean_colvals <- function(df, col) {
  cleaned_col <- 
    select({{col}}) %>% 
    mutate(junk = 1) %>% 
    pivot_wider(
      names_from = {{col}},
      values_from = junk
    ) %>% 
    clean_names() 
    
    df %>% 
    mutate(
      {{col}} := cleaned_col
    )
    
}