datasets_tbl <- tibble::tibble(
  name = c(
    "reported_patient_impact_hospital_capacity_facility",
    "reported_patient_impact_hospital_capacity_state",
    "cpr_national",
    "cpr_county",
    "cases_and_deaths_state_timeseries",
    "pcr_testing_timeseries"
  ),
  url = c(
    "https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD",
    "https://healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD",
    "https://healthdata.gov/api/views/gzn6-r8g2/rows.csv?accessType=DOWNLOAD",
    "https://healthdata.gov/api/views/di4u-7yu6/rows.csv?accessType=DOWNLOAD",
    "https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD",
    "https://healthdata.gov/api/views/j8mb-icvb/rows.csv?accessType=DOWNLOAD"
  )
)

#' @export
download_datasets <- function() {
  if (!dir.exists(data_cache())){
    dir.create(data_cache(), recursive = TRUE)
  }
  
  download_file <- function(url, name) {
    download.file(url, file.path(data_cache(), paste0(name, ".csv")))
  }
  
  invisible(purrr::pmap(datasets_tbl, download_file))
}

get_data <- function() {
  my_read_csv <- function(name) {
    vroom::vroom(file.path(data_cache(), paste0(name, ".csv")))
  }
  
  purrr::map(datasets_tbl$name, my_read_csv) %>% 
    setNames(datasets_tbl$name)
}

data_cache <- function() rappdirs::user_cache_dir("US.covid.dashboard")
