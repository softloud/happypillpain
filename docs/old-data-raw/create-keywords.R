keywords <-
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1vUY0gJ0bl-huVO-QIy7hjXwNRe87rPIt0cO4fLZobEQ/edit#gid=0"
  )

# usethis::use_data(keywords, overwrite=TRUE)

readr::write_csv(keywords, "data-raw/keywords.csv")
