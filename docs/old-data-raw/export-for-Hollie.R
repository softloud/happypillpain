# create an export for Hollie

library(happypillpain)
library(tidyverse)


raw_dat <- raw_hpp_dat_path() %>%
  read_csv(col_types = cols(.default = "c"))

raw_dat %>%
  dplyr::filter(
    `Study Identifier` %in%
      c(raw_dat %>%
        pull(`Study Identifier`) %>%
        unique() %>%
        head(20)
    )
  ) %>% # dim() %>%
  write_csv("data-raw/export-for-hollie.csv")
