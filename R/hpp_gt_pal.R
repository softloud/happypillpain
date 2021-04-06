#' Table options for all tables
#'
#' Sets colour of rowgroups, column header, background.
#'
#' @param tab A [gt] plot object.
#' @param first_col Name of the row-identifier column
#'
#' @export

hpp_gt_pal <- function(tab, first_col){
  tab %>%
    gt::data_color(
      columns = vars(!!first_col),
      colors = "#d9d9d9"
    ) %>%
    gt::tab_options(
    table.background.color = "#f0f0f0",
    column_labels.background.color = "#969696",
    row_group.background.color = "#bdbdbd"
  )
}
