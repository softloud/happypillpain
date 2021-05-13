#' Theme across all tables
#' 
#' @param vertical_divider Dashed vertical line indicating row identifying columns
#' 
#' @export

hpp_tab <- function(df, vertical_divider) {
  df %>% 
  opt_row_striping() %>% 
    tab_options(
      column_labels.background.color = "#ecf2f8",
      row_group.background.color =  "#d9e6f2"
    ) %>% 
    tab_style(
      style = cell_borders(sides = "right", color = "#d9e6f2",
                           style = "dashed", weight = px(3)),
      locations = cells_body(columns = vars({{vertical_divider}}))
    ) %>% 
    tab_style(
      style = cell_borders(sides = "all", color = "#d9e6f2", weight = px(3)),
      locations = cells_column_labels(everything())
    ) 
}