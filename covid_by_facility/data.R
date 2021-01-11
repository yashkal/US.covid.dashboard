# Import ----
replace_with_NA_all <- function(df, formule) {
  df[rlang::as_function(formule)(df)] <- NA
  df
}

df <- vroom("https://healthdata.gov/sites/default/files/reported_hospital_capacity_admissions_facility_level_weekly_average_timeseries_20201221_0.csv") %>% 
  select(-contains("pediatric"))%>%
  replace_with_NA_all(~ .x == -999999)

metrics_df <- df %>% 
  mutate(new_cases_confirmed_or_suspected = previous_day_admission_adult_covid_confirmed_7_day_sum + previous_day_admission_adult_covid_suspected_7_day_sum,
         adult_covid_prop = total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg/ all_adult_hospital_inpatient_beds_7_day_avg,
         staffed_adult_icu_beds_prop = staffed_adult_icu_bed_occupancy_7_day_avg/ total_staffed_adult_icu_beds_7_day_avg,
         staffed_adult_covid_icu_prop = staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg/ total_staffed_adult_icu_beds_7_day_avg) %>% 
  select(hospital_pk:is_metro_micro, new_cases_confirmed_or_suspected:last_col())

county_metrics <- metrics_df %>%
  filter(collection_week == "2020-12-11") %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x) | is.infinite(.x), NA, .x))) %>% 
  group_by(fips_code) %>% 
  summarize(hospital_count = sum(!is.na(new_cases_confirmed_or_suspected)),
            new_cases_confirmed_or_suspected = sum(new_cases_confirmed_or_suspected, na.rm = TRUE))

us_counties <- counties(cb = TRUE, resolution = "20m") %>% 
  left_join(
    fips_codes %>% 
      select(starts_with("state")) %>% 
      distinct() %>% 
      as_tibble(),
    by = c("STATEFP" = "state_code")
  ) %>% 
  mutate(fips_code = paste0(STATEFP, COUNTYFP)) %>% 
  select(NAME, fips_code, state)
