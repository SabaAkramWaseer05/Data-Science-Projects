library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(viridis)
library(maps)
library(countrycode)
library(readr)
library(jsonlite)  # Load jsonlite for fromJSON()

# Check if the JSON file exists
if (!file.exists("data_cia2.json")) {
  stop("Error: The file 'data_cia2.json' was not found in the working directory.")
}

# Load the data
data <- jsonlite::fromJSON("data_cia2.json")

# Prepare map data
world_map <- map_data("world")
world_map$ISO3 <- countrycode::countrycode(sourcevar = world_map$region,
                                           origin = "country.name",
                                           destination = "iso3c", nomatch = NA)

# Join map data with CIA data
data$ISO3 <- countrycode::countrycode(sourcevar = data$country,
                                      origin = "country.name",
                                      destination = "iso3c", nomatch = NA)
map_data <- dplyr::left_join(world_map, data, by = "ISO3")

ui <- fluidPage(
  titlePanel("CIA Factbook Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:", 
                  choices = c("expenditure", "youth_unempl_rate", 
                              "net_migr_rate", "pop_growth_rate", 
                              "electricity_fossil_fuel", "life_expectancy")),
      actionButton("view_data", "View Raw Data"),
      conditionalPanel(
        condition = "input.view_data == 1",
        DTOutput("raw_data_table")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Univariate Analysis",
                 tabsetPanel(
                   tabPanel("Map",
                            plotlyOutput("map_plot")),
                   tabPanel("Global Analysis",
                            plotlyOutput("boxplot"),
                            plotlyOutput("histogram"),
                            plotlyOutput("density")),
                   tabPanel("Analysis per Continent",
                            plotlyOutput("continent_boxplot"),
                            plotlyOutput("continent_density"))
                 )
        ),
        tabPanel("Multivariate Analysis",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("x_var", "X variable:", 
                                 choices = c("expenditure", "youth_unempl_rate", 
                                             "net_migr_rate", "pop_growth_rate", 
                                             "electricity_fossil_fuel", "life_expectancy")),
                     selectInput("y_var", "Y variable:", 
                                 choices = c("expenditure", "youth_unempl_rate", 
                                             "net_migr_rate", "pop_growth_rate", 
                                             "electricity_fossil_fuel", "life_expectancy")),
                     selectInput("size_var", "Size by:", choices = c("population", "area"))
                   ),
                   mainPanel(
                     plotlyOutput("scatterplot")
                   )
                 )
        ),
        tabPanel("Data",
                 DTOutput("full_data_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression for selected data
  selected_data <- reactive({
    data %>% dplyr::select(country, continent, input$variable)
  })
  
  # Render raw data table
  output$raw_data_table <- renderDT({
    datatable(selected_data(), options = list(pageLength = 15))
  })
  
  # Render full data table in the Data tab
  output$full_data_table <- renderDT({
    datatable(data, options = list(pageLength = 15))
  })
  
  # Render map plot
  output$map_plot <- renderPlotly({
    ggplot(map_data, aes(long, lat, group = group, fill = .data[[input$variable]])) +
      geom_polygon(color = "white") +
      scale_fill_viridis_c() +
      theme_minimal() +
      ggtitle(paste("Map of", input$variable))
  })
  
  # Render global analysis plots
  output$boxplot <- renderPlotly({
    ggplot(data, aes(x = "", y = .data[[input$variable]])) +
      geom_boxplot() +
      theme_minimal() +
      ggtitle(paste("Boxplot of", input$variable))
  })
  
  output$histogram <- renderPlotly({
    ggplot(data, aes(x = .data[[input$variable]])) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      theme_minimal() +
      ggtitle(paste("Histogram of", input$variable))
  })
  
  output$density <- renderPlotly({
    ggplot(data, aes(x = .data[[input$variable]])) +
      geom_density(fill = "skyblue") +
      theme_minimal() +
      ggtitle(paste("Density Plot of", input$variable))
  })
  
  # Render analysis per continent plots
  output$continent_boxplot <- renderPlotly({
    ggplot(data, aes(x = continent, y = .data[[input$variable]], fill = continent)) +
      geom_boxplot() +
      theme_minimal() +
      ggtitle(paste("Boxplot by Continent for", input$variable))
  })
  
  output$continent_density <- renderPlotly({
    ggplot(data, aes(x = .data[[input$variable]], fill = continent)) +
      geom_density(alpha = 0.5) +
      theme_minimal() +
      ggtitle(paste("Density Plot by Continent for", input$variable))
  })
  
  # Render scatterplot for multivariate analysis
  output$scatterplot <- renderPlotly({
    ggplot(data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], 
                     color = continent, size = .data[[input$size_var]])) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "loess", se = FALSE, aes(group = continent), color = "black") +
      theme_minimal() +
      ggtitle(paste("Scatterplot of", input$x_var, "vs", input$y_var))
  })
}

shinyApp(ui, server)