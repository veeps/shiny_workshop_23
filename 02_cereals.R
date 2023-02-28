library(shiny)
library(DT)
library(readr)
library(dplyr)
library(plotly)

# Read in data ---------------------------------------------------------

df <- read.csv("data/cereal.csv") |> select(-Manufacturer)
nutrient_dense <- c( "Carbs", "Fiber",  "Potassium","Protein", "Vitamins")

# UI section ------------------------------------------------------------


ui <- fluidPage(
  
  # Header section -------------------------------------------------------
  
  
  fluidRow(style="padding:40px; background: #03bf7b; color: #ffffff; text-align:center",
           icon("bowl-rice", "fa-2xl", lib = "font-awesome"),
           h1("Adult Cereal Selector")
  ), # end fluid row
  
  fluidRow(style="padding:40px; background: #f2f2f2",
           h3("Nutritional Density ", align="center"),
           p("Let's start by ranking cereals based on their nutritional density. Which ones have more of the 'good stuff'? ", align="center")
  ), # end fluid row
  
  # Bar chart section -------------------------------------------------------
  
  # Data table section ----------------------------------------------------
  
  # Scatter plot section --------------------------------------------------
  
  
) # end UI


# Server section -------------------------------------------------------

server <- function(input, output, session) {
  
  # Bar chart section -------------------------------------------------------
  
  # render barchart
  output$bar_plot <- renderPlot({
    ggplot(df |> arrange(desc(Protein)) |> head(10),
           aes(y=Protein, 
               x=Name)) + 
      geom_bar(stat="identity", fill = "#0add8c") +
      theme(axis.title.y=element_blank())
  })

  # Data table section ----------------------------------------------------
  
  # render table
  # output$table <- renderDT()
  
  # Scatter plot section --------------------------------------------------
  
  # render scatterplot
  # output$scatter_plot <- renderPlotly({ })

  
}

shinyApp(ui, server)