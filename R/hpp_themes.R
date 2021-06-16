#' Theme colours
#' 
#' @param colour Colour

hpp_pal <- function(colour) {
  colours <- 
    c("#ecf2f8", "#d9e6f2")
  
  colours[colour]
}



#' Theme across all tables
#' 
#' @param vertical_divider Dashed vertical line indicating row identifying columns
#' 
#' @export

hpp_tab <- function(gt_tab, vertical_divider = NULL) {
  tab <- 
    gt_tab %>%
  opt_row_striping() %>% 
    tab_options(
      column_labels.background.color = hpp_pal(1),
      row_group.background.color =  hpp_pal(2)
    ) %>% 
    tab_style(
      style = cell_borders(sides = "all", color = hpp_pal(2), weight = px(3)),
      locations = cells_column_labels(everything())
    ) 
  
  if (is.null(vertical_divider)) {
    tab
  } else {
    tab %>% 
      tab_style(
        style = cell_borders(sides = "right", color = hpp_pal(2),
                             style = "dashed", weight = px(3)),
        locations = cells_body(columns ={{vertical_divider}})
      ) 
      
  }
}