# get outcome measures

library(googlesheets4)

scales <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1Q8QuZ9avto7TlBvHMpcPyX0NFI3R9nLevVjqBXDlHGg/edit#gid=1910672474",
             col_types = "c")

write_csv(
  scales, "data-raw/scales.csv"
)
