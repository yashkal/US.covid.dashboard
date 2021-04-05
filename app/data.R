# Import ----
df <- vroom("data/reported_weekly_facility_level_capacity.csv") %>% 
  select(-contains("pediatric"))

# Select hospitals that have reported 95% of days
frequently_reported_hospitals <- df %>% 
  count(hospital_pk) %>% 
  filter(n >= 0.95*max(n)) %>% 
  pull(hospital_pk)

df <- df %>% 
  filter(hospital_pk %in% frequently_reported_hospitals)

# Augment hospital name for duplicates
augmented_names <- df %>% 
  distinct(hospital_pk, hospital_name, city, state) %>% 
  add_count(hospital_name) %>% 
  filter(n > 1) %>% 
  arrange(desc(n), hospital_name) %>% 
  mutate(augmented_name = as.character(glue::glue("{hospital_name} ({city}, {state})"))) %>% 
  select(hospital_pk, augmented_name)

df <- df %>% 
  left_join(augmented_names) %>% 
  mutate(augmented_name = if_else(is.na(augmented_name), hospital_name, augmented_name))

# Calculate metrics
metrics_df <- df %>% 
  mutate(new_cases_confirmed_or_suspected = previous_day_admission_adult_covid_confirmed_7_day_sum + previous_day_admission_adult_covid_suspected_7_day_sum,
         adult_covid_prop = total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg/ all_adult_hospital_inpatient_beds_7_day_avg,
         staffed_adult_icu_beds_prop = staffed_adult_icu_bed_occupancy_7_day_avg/ total_staffed_adult_icu_beds_7_day_avg,
         staffed_adult_covid_icu_prop = staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg/ total_staffed_adult_icu_beds_7_day_avg) %>% 
  select(hospital_pk:is_metro_micro, augmented_name, new_cases_confirmed_or_suspected:last_col())

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
