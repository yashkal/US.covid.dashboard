library(tidyverse)
library(vroom)
library(shiny)
library(shinycssloaders)
library(sf)
library(tigris)
library(tmap)

theme_set(theme_bw())

source("data.R")

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
            sort(unique(metrics_df$augmented_name)))
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
            filter(augmented_name == input$facility) %>% 
            ggplot(aes(x = collection_week, y = adult_covid_prop)) +
            geom_point() +
            geom_line() +
            scale_y_continuous(labels = scales::label_percent()) +
            labs(title = "How full is the hospital with adult confirmed and suspected COVID patients?", x = NULL, y = NULL)
    })
}

shinyApp(ui = ui, server = server)