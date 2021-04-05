library(tidyverse)
library(vroom)

replace_with_NA_all <- function(df, formule) {
  df[rlang::as_function(formule)(df)] <- NA
  df
}

fp <- tempfile()

download.file(
  url = "https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD",
  destfile = fp
)

df <- vroom(fp) %>% 
  replace_with_NA_all(~ .x == -999999)

write_csv(df, "data/reported_weekly_facility_level_capacity.csv")
write_csv(df, "app/data/reported_weekly_facility_level_capacity.csv")