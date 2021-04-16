data_url <- "https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD"

download.file(
  url = data_url,
  destfile = "data/reported_weekly_facility_level_capacity.csv"
)

file.copy(from = "data/reported_weekly_facility_level_capacity.csv",
          to = "app/data/reported_weekly_facility_level_capacity.csv",
          overwrite = TRUE)