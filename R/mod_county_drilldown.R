metrics_df <- tibble(
  var = c(
    "cases_last_7_days", 
    #"cases_per_100k_last_7_days", "cases_pct_change_from_prev_week",
    "deaths_last_7_days"
    # #"deaths_per_100k_last_7_days", "deaths_pct_change_from_prev_week",
    # "test_positivity_rate_last_7_days", #"test_positivity_rate_pct_change_from_prev_week",
    # "confirmed_covid_hosp_per_100_beds_last_7_days", "confirmed_covid_hosp_per_100_beds_pct_change_from_prev_week",
    # "pct_inpatient_beds_used_covid_avg_last_7_days", "pct_inpatient_beds_used_covid_abs_change_from_prev_week",
    # "pct_icu_beds_used_covid_avg_last_7_days", "pct_icu_beds_used_covid_abs_change_from_prev_week"
  ),
  title = c(
    "Cases", 
    #"Cases per 100k", "% Change in Cases per 100k",
    "Deaths"
    # #"Deaths per 100k", "% Change in Cases per 100k",
    # "Viral (RT-PCR) Lab Test Positivity", 
    # #"Change in Viral Lab Test Positivity",
    # "COVID-19 Confirmed Hospital Admissions per 100 beds", "% Change in Confirmed COVID-19 Admissions per 100 beds",
    # "Hospital Inpatient Utilization for COVID-19 Patients", "Change in Inpatient Utilization for COVID-19 Patients",
    # "Adult ICU Utilization for COVID-19 Patients", "Change in Adult ICU Utilization for COVID-19 Patients"
  ),
  breaks = list(
    c(0, 1, 10, 100, 1000, 5000, 1e4, Inf),
    #c(0, 5, 10, 50, 100, 200, 500, 750, Inf)*100e3, c(-Inf, -.25, -.1, 0, .1, 1, 10, Inf),
    c(0, 1, 5, 25, 100, Inf)
    # # c(0, 0.1, 1, 2, 5, 10, 15, Inf), c(-Inf, -.25, -.1, 0, .1, .25, Inf),
    # c(0, .03, .05, .08, .1, .15, .2, .25, Inf),
    # #c(-Inf, -.02, .005, 0, 0.005, .02, Inf),
    # c(0, 2, 4, 6, 11, 16, 21, Inf),
    # c(-Inf, -.25, -.1, 0, .1, .25, Inf),
    # c(0, 4, 8, 13, 16, 21, 26, 31, Inf)/100,
    # c(-Inf, 2, 1, 0, 1, 2, 3, Inf)/100,
    # c(0, 4, 8, 13, 16, 21, 26, 31, Inf)/100,
    # c(-Inf, 2, 1, 0, 1, 2, 3, Inf)/100
  ),
  color_palette = list(
    c("#f0f0f0", tmaptools::get_brewer_pal("YlOrBr", n = 6, contrast = c(0.24, 0.85), plot = FALSE)),
    c("#f0f0f0", tmaptools::get_brewer_pal("YlOrBr", n = 4, contrast = c(0.24, 0.85), plot = FALSE))
  )
)

us_states <- tigris::states(cb = TRUE, resolution = "20m", progress_bar = FALSE)
conus <- us_states %>% 
  filter(NAME %in% setdiff(state.name, c("Hawaii", "Alaska"))) %>% 
  sf::st_bbox()

drilldownPlotUI <- function(id, label = "drilldown") {
  ns <- NS(id)
  sidebarLayout(
    box(
      selectInput(ns("state"), "Choose state", setNames(c("All", state.abb), c("All", state.name))),
      selectInput(ns("metric"), "Choose metric",
                  setNames(metrics_df$var, metrics_df$title)),
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
          left_join(cpr_county, by = "fips") %>%
          filter(if (input$state == "All") TRUE else input$state == state) %>% 
          tm_shape(bbox = if (input$state == "All") conus else NULL) +
          tm_polygons(input$metric,
                      title = metrics_df %>% filter(var == input$metric) %>% pull(title),
                      breaks = metrics_df %>% filter(var == input$metric) %>% pull(breaks) %>% pluck(1),
                      palette = metrics_df %>% filter(var == input$metric) %>% pull(color_palette) %>% pluck(1),
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
