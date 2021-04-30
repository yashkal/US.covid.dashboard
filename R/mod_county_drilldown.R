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

drilldownPlotUI <- function(id, label = "drilldown") {
  ns <- NS(id)
  sidebarLayout(
    box(
      selectInput(ns("state"), "Choose state", setNames(state.abb, state.name)),
      selectInput(ns("metric"), "Choose metric", metric_choices),
      width = 3
    ),
    box(tmap::tmapOutput(ns("plotStateCountyDrilldown")), width = 9)
  )
}

drilldownPlotServer <- function(id) {
  cpr_county <- get_data()$cpr_county
  us_counties <- tigris::counties(cb = TRUE, resolution = "20m", progress_bar = FALSE) %>% 
    mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP)))

  moduleServer(
    id,
    function(input, output, session) {
      output$plotStateCountyDrilldown <- tmap::renderTmap({
        us_counties %>% 
          left_join(cpr_county) %>%
          filter(input$state == state) %>% 
          tm_shape() +
          tm_polygons(input$metric,
                      title = names(metric_choices)[which(input$metric == metric_choices)],
                      id = "NAME")
      })
    }
  )
}

drilldownPlotApp <- function() {
  ui <- fluidPage(drilldownPlotUI("ex1"))
  server <- function(input, output, session) {
    drilldownPlotServer("ex1")
  }
  shinyApp(ui, server)
}
