plot_historical_hospital_admissions <- shiny::reactive({
  reported_patient_impact_hospital_capacity_state <- get_data()$reported_patient_impact_hospital_capacity_state
  
  reported_patient_impact_hospital_capacity_state %>% 
    filter(date > lubridate::mdy("Aug 01 2020")) %>% 
    select(state, date,
           previous_day_admission_adult_covid_confirmed,
           previous_day_admission_pediatric_covid_confirmed,
           previous_day_admission_adult_covid_suspected,
           previous_day_admission_pediatric_covid_suspected) %>%
    group_by(date) %>% 
    summarize(confirmed = sum(previous_day_admission_adult_covid_confirmed, previous_day_admission_pediatric_covid_confirmed, na.rm = TRUE),
              suspected = sum(previous_day_admission_adult_covid_suspected, previous_day_admission_pediatric_covid_suspected, na.rm = TRUE)) %>% 
    pivot_longer(c(confirmed, suspected), names_to = "type", values_to = "cases") %>% 
    mutate(type = str_to_title(type),
           type = fct_relevel(type, c("Suspected", "Confirmed"))) %>% 
    ggplot(aes(x = date, y = cases, fill = type)) + 
    geom_col(width = 1, alpha = 0.75) +
    scale_x_date(minor_breaks = "month", date_labels = "%b '%y") +
    scale_y_continuous(labels = scales::comma) + 
    scale_fill_manual(values = c("gray75", "gray20")) +
    expand_limits(y = 0) +
    labs(title = "New Hospital Admissions Based on COVID Status",
         y = NULL, x = NULL, fill = NULL)
})

plot_historical_pcr_testing <- shiny::reactive({
  pcr_testing_timeseries <- get_data()$pcr_testing_timeseries
  
  pcr_testing_timeseries %>%
    group_by(date, overall_outcome) %>% 
    summarize(new_results_reported = sum(new_results_reported)) %>% 
    arrange(desc(date), overall_outcome) %>% 
    add_tally(new_results_reported) %>% 
    filter(overall_outcome == "Positive") %>% 
    mutate(test_positivity = new_results_reported/n) %>% 
    ggplot(aes(x = date, y = test_positivity)) +
    geom_col(width = 1, alpha = 0.75) +
    scale_x_date(minor_breaks = "month", date_labels = "%b '%y") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = NULL, title = "Viral (RT-PCR) Lab Test Positivity")
})

plot_historical_deaths <- shiny::reactive({
  cases_and_death_state_timeseries <- get_data()$cases_and_deaths_state_timeseries %>% 
    mutate(submission_date = lubridate::mdy(submission_date)) 
  
  cases_and_death_state_timeseries %>%
    filter(submission_date > lubridate::mdy("Mar 1 2020")) %>% 
    select(1:2, new_death) %>% 
    arrange(desc(submission_date), state, new_death) %>%
    count(submission_date, wt = new_death) %>% 
    arrange(submission_date) %>% 
    ggplot(aes(x = submission_date, y = n)) +
    geom_col(width = 1, alpha = 0.75) +
    scale_x_date(minor_breaks = "month", date_labels = "%b '%y") +
    labs(x = NULL, y = NULL, title = "Deaths Due to Coronavirus")
})

plot_historical_new_cases <- shiny::reactive({
  cases_and_death_state_timeseries <- get_data()$cases_and_deaths_state_timeseries %>% 
    mutate(submission_date = lubridate::mdy(submission_date))
  
  cases_and_death_state_timeseries %>%
    filter(submission_date > lubridate::mdy("Mar 1 2020")) %>% 
    select(1:2, new_case) %>% 
    arrange(desc(submission_date), state, new_case) %>%
    # filter(submission_date == min(submission_date)) %>%
    count(submission_date, wt = new_case) %>% 
    arrange(submission_date) %>% 
    ggplot(aes(x = submission_date, y = n)) +
    geom_col(width = 1, alpha = 0.75) +
    scale_x_date(minor_breaks = "month", date_labels = "%b '%y") +
    scale_y_continuous(labels = scales::label_number(scale = 1/1e3, suffix = "k")) +
    labs(x = NULL, y = NULL, title = "New COVID-19 Cases")
})

