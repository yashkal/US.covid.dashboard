library(tidyverse)
library(vroom)
library(shiny)
library(shinycssloaders)
library(sf)
library(tigris)
library(tmap)

theme_set(theme_bw())

# Import ----
replace_with_NA_all <- function(df, formule) {
    df[rlang::as_function(formule)(df)] <- NA
    df
}

df <- vroom("https://healthdata.gov/sites/default/files/reported_hospital_capacity_admissions_facility_level_weekly_average_timeseries_20201221_0.csv") %>% 
    select(-contains("pediatric"))%>%
    replace_with_NA_all(~ .x == -999999)

metrics_df <- df %>% 
    mutate(
        has_adult_inpatient_beds = all_adult_hospital_inpatient_beds_7_day_avg > 0,
        has_staffed_adult_icu_beds = total_staffed_adult_icu_beds_7_day_avg > 0,
        new_cases_confirmed_or_suspected = sum(previous_day_admission_adult_covid_confirmed_7_day_sum, na.rm = TRUE),
        adult_covid_prop =
            total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg/ all_adult_hospital_inpatient_beds_7_day_avg,
        staffed_adult_icu_beds_prop = staffed_adult_icu_bed_occupancy_7_day_avg/ total_staffed_adult_icu_beds_7_day_avg,
        staffed_adult_covid_icu_prop = staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg/ total_staffed_adult_icu_beds_7_day_avg) %>% 
    select(hospital_pk:is_metro_micro, has_adult_inpatient_beds:last_col())

county_metrics <- metrics_df %>%
    filter(collection_week == "2020-12-11") %>% 
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x) | is.infinite(.x), NA, .x))) %>% 
    group_by(fips_code) %>% 
    summarize(
        hospital_count = sum(!is.na(new_cases_confirmed_or_suspected)),
        new_cases_confirmed_or_suspected = sum(new_cases_confirmed_or_suspected, na.rm = TRUE)
    )

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

# Application ----

state_maps <- sidebarLayout(
    sidebarPanel(
        selectInput(
            "state", "Choose state:",
            sort(unique(us_counties$state)))
    ),
    mainPanel(tmapOutput("county_plot") %>% withSpinner())
)

facility_plots <- sidebarLayout(
    sidebarPanel(
        selectInput(
            "facility", "Choose facility:",
            sort(unique(metrics_df$hospital_name)))
    ),
    mainPanel(plotOutput("time_series_plot") %>% withSpinner())
)

ui <- fluidPage(
    titlePanel("Covid Tracker by Facility"),
    tabsetPanel(type = "tabs",
                tabPanel("state_maps", state_maps),
                tabPanel("facility_plots", facility_plots))
)

server <- function(input, output) {
    output$county_plot <- renderTmap({
        us_counties %>% 
            filter(state == input$state) %>%
            left_join(county_metrics, by = "fips_code") %>% 
            tm_shape() +
            tm_polygons("new_cases_confirmed_or_suspected")
    })
    
    output$time_series_plot <- renderPlot({
        metrics_df %>% 
            select(collection_week, hospital_name, is_metro_micro:last_col()) %>% 
            filter(hospital_name == input$facility) %>% 
            ggplot(aes(x = collection_week, y = adult_covid_prop)) +
            geom_point() +
            geom_line() +
            scale_y_continuous(labels = scales::label_percent()) +
            labs(title = "How full is the hospital with adult confirmed and suspected COVID patients?", x = NULL, y = NULL)
    })
}

shinyApp(ui = ui, server = server)