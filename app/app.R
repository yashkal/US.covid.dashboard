## app.R ##
library(tidyverse)
library(vroom)
library(shiny)
library(shinydashboard)

# Setup ----
shinyOptions(plot.autocolors = TRUE)
theme_set(theme_bw(base_size = 16))

# Load data
state_utilization_timeseries <- vroom(here::here("data", "reported_patient_impact_hospital_capacity_state.csv"))
cpr_national <- vroom(here::here("data", "cpr_national.csv"))
latest_cases_deaths <- vroom(here::here("data", "cases_and_deaths_state_timeseries.csv")) %>% 
    mutate(submission_date = lubridate::mdy(submission_date)) %>% 
    filter(submission_date >= max(submission_date) - lubridate::days(7))

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

# Plots ----
plot_new_admissions <- reactive({
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
        geom_col(width = 1) +
        scale_x_date(date_breaks = "month", minor_breaks = NULL,
                     date_labels = "%b %y") +
        scale_y_continuous(labels = scales::comma) + 
        expand_limits(y = 0) +
        labs(title = "New Hospital Admissions (Daily)",
             y = "Cases", x = NULL, fill = "Status")
})

# User Interface ----

body <- dashboardBody(
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
    plotOutput("plotNewAdmissions"))

ui <- dashboardPage(
    dashboardHeader(title = "COVID 19 Dashboard"),
    dashboardSidebar(),
    body
)

# Server ----
server <- function(input, output) {
    output$plotNewAdmissions <- renderPlot(plot_new_admissions())
}

# Run app ----
shinyApp(ui, server)
