summaryInfoBox <- function(title, value, icon_name, format_fun){
  infoBox(title = title, value = format_fun(value), icon = icon(icon_name))
}

info_tbl <- function() {
  cpr_national <- get_data()$cpr_national
  
  cases_and_death_state_timeseries <- get_data()$cases_and_deaths_state_timeseries %>% 
    mutate(submission_date = lubridate::mdy(submission_date))
  
  latest_cases_deaths <- cases_and_death_state_timeseries %>% 
    filter(submission_date >= max(submission_date) - lubridate::days(7))
  
  national_summary_stats <- cpr_national %>% 
    mutate(
      tot_cases = latest_cases_deaths %>%
        filter(submission_date == max(submission_date)) %>% 
        pull(tot_cases) %>% 
        sum(),
      tot_death = latest_cases_deaths %>% 
        filter(submission_date == max(submission_date)) %>% 
        pull(tot_death) %>% 
        sum()
    )
  
  info_tbl <- tibble(
    title = c("Total Cases", "New Cases (Last 7 days)", "Percent Change (Last 7 days)", "Total Deaths", "New Deaths (Last 7 days)", "Percent Change (Last 7 days)"),
    value = c(national_summary_stats$tot_cases, national_summary_stats$cases_last_7_days, national_summary_stats$cases_pct_change_from_previous_week, national_summary_stats$tot_death,national_summary_stats$deaths_last_7_days, national_summary_stats$deaths_pct_change_from_prev_week),
    icon_name = c("lungs-virus", "bed", "percent", "book-dead", "dizzy", "percent"),
    format_fun = c(scales::comma_format(), scales::comma_format(), scales::percent_format(accuracy = 0.01), scales::comma_format(), scales::comma_format(), scales::percent_format(accuracy = 0.01))
  )
}

