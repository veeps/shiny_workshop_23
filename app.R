library(tidyverse)
library(magrittr)
library(plotly)
library(shiny)
library(shiny)

ui <- fluidPage(
  fluidRow(plotOutput("total_bar")),
  fluidRow(plotlyOutput("masters_demo"))

)

server <- function(input, output, session) {
  
  # read in data on masters degree
  masters <- read_csv("masters_degree_conferred_usafacts_clean.csv")
  
  # create a bar chart of total grads
  output$total_bar <- renderPlot({
    ggplot({masters |> select(year, total) |> distinct()},
           aes(x=year,
               y=total)) + geom_bar(stat="identity")
  })
  
  # plotly time series chart by race_ethnicity
  output$masters_demo <- renderPlotly({
    plot_ly(
      data=masters,
      x=~year,
      y=~percent,
      color=~race_ethnicity,
      type='scatter',
      mode = 'lines'
    ) |>
      layout(
        xaxis=list(title="Year"),
        yaxis=list(title="Percent of All Students"),
        title = "Master's Degree Conferred in the US by Race/Ethnicity"
      )
  })
  
  
}

shinyApp(ui, server)