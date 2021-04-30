## app.R ##
# library(tidyverse)
# library(vroom)
library(shiny)
library(shinydashboard)
# library(sf)
# library(tigris)
library(tmap)

runApp <- function(host = '0.0.0.0', port = 3838){
  shiny::runApp(app(), host = host, port = port)
}

app <- function(){
  # Setup ----
  shiny::shinyOptions(plot.autocolors = TRUE)
  theme_set(theme_bw(base_size = 16))
  covid_datasets <- get_data() # run download_datasets() to get latest data
  
  # Load data
  reported_patient_impact_hospital_capacity_state <- covid_datasets$reported_patient_impact_hospital_capacity_state
  cpr_national <- covid_datasets$cpr_national
  cases_and_death_state_timeseries <- covid_datasets$cases_and_deaths_state_timeseries %>% 
    mutate(submission_date = lubridate::mdy(submission_date))
  latest_cases_deaths <- cases_and_death_state_timeseries %>% 
    filter(submission_date >= max(submission_date) - lubridate::days(7))
  us_states <- tigris::states(cb = TRUE, resolution = "20m", progress_bar = FALSE)
  conus <- us_states %>% 
    filter(NAME %in% setdiff(state.name, c("Hawaii", "Alaska"))) %>% 
    sf::st_bbox()
  
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
  
  state_utilization_metrics <-  reported_patient_impact_hospital_capacity_state %>% 
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
  
  # User Interface ----
  shortage_choices <- setNames(
    c("percent_staffing_shortage_today", "percent_staffing_shortage_anticipated_within_week"),
    c("% Staffing Shortage Today", "% Anticipated Shortage Next Week")
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
          box(tmap::tmapOutput("plotStaffingShortages"), width = 12)
        )
      ),
      tabItem(
        tabName = "count_state_drilldown",
        drilldownPlotUI("ex1")
      ),
      tabItem(
        tabName = "history",
        h1("Historical Trends"),
        fluidRow(
          box(shinycssloaders::withSpinner(plotOutput("plotNewCases"))),
          box(shinycssloaders::withSpinner(plotOutput("plotPCRTesting")))
        ),
        fluidRow(
          box(shinycssloaders::withSpinner(plotOutput("plotDeaths"))),
          box(shinycssloaders::withSpinner(plotOutput("plotNewAdmissions")))
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
    output$plotStaffingShortages <- tmap::renderTmap({
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
    drilldownPlotServer("ex1")
  }
  
  shinyApp(ui, server)
}