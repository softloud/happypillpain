#' Get the latest raw data
#'
#' Read from the data file: latest version of the raw data exported from
#' Covidence.
#'
#' @param now this is just here so I can figure out how to update data in targets.
#'
#' @export

raw_hpp_dat_path <- function(now = lubridate::now(), datadir = "data") {
  # read in file names from data
  raw_data_files <-
    list.files(here::here(datadir), pattern = "review_91309_extracted.*csv")
  
  # extract dates
  raw_data_dates <-
    stringr::str_extract(raw_data_files, "\\d{14}") %>%
    lubridate::ymd_hms()
  
  # calculate most recent
  diff_dates <- now - raw_data_dates
  most_recent <- which(diff_dates == min(diff_dates))
  
  # read data frame
  # suppressWarnings( # there will be duplicates
  file.path(here::here(datadir, raw_data_files[most_recent])) #%>%
  # readr::read_csv(col_types = cols(.default = "c"))
  # )
  
}

#' Check what date the latest file is
#'
#' @param path From [raw_hpp_dat_path].
#'
#' @export

raw_hpp_dat_date <- function(path) {
  path %>%  stringr::str_extract("\\d{14}") %>% lubridate::ymd_hms()
}
