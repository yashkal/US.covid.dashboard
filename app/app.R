## app.R ##
library(tidyverse)
library(vroom)
library(shiny)
library(shinydashboard)
library(sf)
library(tigris)
library(tmap)

# Setup ----
shinyOptions(plot.autocolors = TRUE)
theme_set(theme_bw(base_size = 16))

# Load data
state_utilization_timeseries <- vroom(here::here("data", "reported_patient_impact_hospital_capacity_state.csv"))
cpr_national <- vroom(here::here("data", "cpr_national.csv"))
pcr_testing_timeseries <- vroom(here::here("data", "pcr_testing_timeseries.csv"))
cases_and_death_state_timeseries <- vroom(here::here("data", "cases_and_deaths_state_timeseries.csv")) %>% 
    mutate(submission_date = lubridate::mdy(submission_date))
latest_cases_deaths <- cases_and_death_state_timeseries %>% 
    filter(submission_date >= max(submission_date) - lubridate::days(7))
df_tbl <- vroom(here::here("data", "cpr_county.csv"))
us_counties <- counties(cb = TRUE, resolution = "20m", progress_bar = FALSE) %>% 
    mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP)))

# Calculate Statistics
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

state_utilization_metrics <-  state_utilization_timeseries %>% 
    filter(date == max(date)) %>% 
    # Staffing shortage metrics
    mutate(
        percent_staffing_shortage_today = critical_staffing_shortage_today_yes/(critical_staffing_shortage_today_yes + critical_staffing_shortage_today_no),
        percent_staffing_shortage_anticipated_within_week = critical_staffing_shortage_anticipated_within_week_yes/(critical_staffing_shortage_anticipated_within_week_yes + critical_staffing_shortage_anticipated_within_week_no)
    ) %>% 
    # For labels
    mutate(
        shortage = paste(critical_staffing_shortage_today_yes, critical_staffing_shortage_today_yes +  critical_staffing_shortage_today_no, sep = "/"),
        anticipated_shortage = paste(critical_staffing_shortage_anticipated_within_week_yes, critical_staffing_shortage_anticipated_within_week_yes +  critical_staffing_shortage_anticipated_within_week_no, sep = "/"),
    )

# Plots ----
us_states <- states(cb = TRUE, resolution = "20m", progress_bar = FALSE)

conus <- us_states %>% 
    filter(NAME %in% setdiff(state.name, c("Hawaii", "Alaska"))) %>% 
    st_bbox()

plot_historical_hospital_admissions <- reactive({
    state_utilization_timeseries %>% 
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

plot_historical_pcr_testing <- reactive({
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

plot_historical_deaths <- reactive({
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

plot_historical_new_cases <- reactive({
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

# User Interface ----
shortage_choices <- setNames(
    c("percent_staffing_shortage_today", "percent_staffing_shortage_anticipated_within_week"),
    c("% Staffing Shortage Today", "% Anticipated Shortage Next Week")
)

metric_choices <- setNames(
    c("cases_last_7_days", "cases_per_100k_last_7_days", "cases_pct_change_from_prev_week",
      "deaths_last_7_days", "deaths_per_100k_last_7_days", "deaths_pct_change_from_prev_week",
      "test_positivity_rate_last_7_days", "test_positivity_rate_pct_change_from_prev_week",
      "confirmed_covid_hosp_per_100_beds_last_7_days", "confirmed_covid_hosp_per_100_beds_pct_change_from_prev_week",
      "pct_inpatient_beds_used_covid_avg_last_7_days", "pct_inpatient_beds_used_covid_abs_change_from_prev_week",
      "pct_icu_beds_used_covid_avg_last_7_days", "pct_icu_beds_used_covid_abs_change_from_prev_week"),
    c("Cases", "Cases per 100k", "% Change in Cases per 100k",
      "Deaths", "Deaths per 100k", "% Change in Cases per 100k",
      "Viral (RT-PCR) Lab Test Positivity", "Change in Viral Lab Test Positivity",
      "COVID-19 Confirmed Hospital Admissions per 100 beds", "% Change in Confirmed COVID-19 Admissions per 100 beds",
      "Hospital Inpatient Utilization for COVID-19 Patients", "Change in Inpatient Utilization for COVID-19 Patients",
      "Adult ICU Utilization for COVID-19 Patients", "Change in Adult ICU Utilization for COVID-19 Patients")
)


body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "summary",
            h1("Weekly Summary"),
            fluidRow(
                infoBox(
                    title = "Total Cases",
                    value = national_summary_stats$tot_cases,
                    icon = icon("lungs-virus")
                ),
                infoBox(
                    title = "New Cases (Last 7 days)",
                    value = national_summary_stats$cases_last_7_days,
                    icon = icon("bed")
                ),
                infoBox(
                    title = "Percent Change (Last 7 days)",
                    value = national_summary_stats$cases_pct_change_from_previous_week,
                    icon = shiny::icon("percent")
                ),
                infoBox(
                    title = "Total Deaths",
                    value = national_summary_stats$tot_death,
                    icon = icon("book-dead")
                ),
                infoBox(
                    title = "New Deaths (Last 7 days)",
                    value = national_summary_stats$deaths_last_7_days,
                    icon = icon("dizzy")
                ),
                infoBox(
                    title = "Percent Change (Last 7 days)",
                    value = national_summary_stats$deaths_pct_change_from_prev_week,
                    icon = shiny::icon("percent")
                )
            ),
            fluidPage(
                h1("Staffing Shortages"),
                box(radioButtons("expected_shortage", NULL, choices = shortage_choices)),
                box(tmapOutput("plotStaffingShortages"), width = 12)
            )
        ),
        tabItem(
            tabName = "history",
            h1("Historical Trends"),
            fluidRow(
                box(plotOutput("plotNewCases")),
                box(plotOutput("plotPCRTesting"))
            ),
            fluidRow(
                box(plotOutput("plotDeaths")),
                box(plotOutput("plotNewAdmissions"))
            )
        ),
        tabItem(
            tabName = "count_state_drilldown",
            sidebarLayout(
                box(
                    selectInput("state", "Choose state", setNames(state.abb, state.name)),
                    selectInput("metric", "Choose metric", metric_choices),
                    width = 3
                ),
                box(tmapOutput("plotStateCountyDrilldown"), width = 9)
            )
        )
    )
)

ui <- dashboardPage(
    dashboardHeader(title = "COVID 19 Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Summary", tabName = "summary"),
            menuItem("County/State Drilldown", tabName = "count_state_drilldown"),
            menuItem("Historical Trends", tabName = "history")
        )
    ),
    body
)

# Server ----
server <- function(input, output) {
    output$plotNewAdmissions <- renderPlot(plot_historical_hospital_admissions())
    output$plotNewCases <- renderPlot(plot_historical_new_cases())
    output$plotDeaths <- renderPlot(plot_historical_deaths())
    output$plotPCRTesting <- renderPlot(plot_historical_pcr_testing())
    output$plotStaffingShortages <- renderTmap({
        us_states %>% 
            left_join(state_utilization_metrics, by = c("STUSPS" = "state")) %>% 
            tm_shape(bbox = conus) +
            tm_polygons(input$expected_shortage, 
                        title = names(shortage_choices)[which(input$expected_shortage == shortage_choices)],
                        id = "NAME", palette = "Oranges",
                        popup.vars = c(
                            "Hospitals with staffing shortage" = "shortage",
                            "Hospitals anticipating shortage" = "anticipated_shortage"))
    })
    output$plotStateCountyDrilldown <- renderTmap({
        us_counties %>% 
            left_join(df_tbl) %>%
            filter(input$state == state) %>% 
            tm_shape() +
            tm_polygons(input$metric,
                        title = names(metric_choices)[which(input$metric == metric_choices)],
                        id = "NAME")
    })
}

# Run app ----
shinyApp(ui, server)
