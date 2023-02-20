library(shiny)
library(DT)
library(readr)
library(dplyr)
library(plotly)

# read in data 
df <- read.csv("data/cereal_2.csv") 


ui <- fluidPage(
  
  fluidRow(style="padding:40px; background: #00ffa2",
    h1("Cereal Selector.. Aka, Adulting is Hard :( ")
  ),
  
  # Bar chart section -------------------------------------------------------
  
  # create side panel with dropdown menu
  fluidRow(style="padding:40px",
    column(3,radioButtons(inputId="bar_var", #references the input to server
                         label = h3("Select Variable"), # text that appears on UI
                         choices=colnames(df)[3:11] |> sort())),
    # plot bar chart
    column(9,plotOutput("bar_plot"))
    ),# end fluidRow
  
  # Scatter plot section --------------------------------------------------
  
  # create side panel with radio buttons
  fluidRow(style="padding:40px; background: #f2f2f2",
           column(3,div(selectInput(inputId="xaxis", #references the input to server
                                label = h3("Select X Variable"), # text that appears on UI
                                choices=colnames(df)[3:11] |> sort(),
                                selected="Calories"),
                        selectInput(inputId="yaxis", #references the input to server
                                    label = h3("Select Y Variable"), # text that appears on UI
                                    choices=colnames(df)[3:11] |> sort(),
                                    selected="Sugars")
                       ) #end div
                  ), # end column
           # plot bar chart
           column(9,plotlyOutput("scatter_plot"))
  ),# end fluidRow
  
  
  # Data table section ----------------------------------------------------
  
  fluidRow(style="padding:40px",
    DTOutput("table")
  )
  
  
) # end UI

server <- function(input, output, session) {
  
  
  # create summary table of median values
  summary <- df |>
    summarise_if(is.numeric, median, na.rm = TRUE)
  
  # render barchart
  output$bar_plot <- renderPlot({
    ggplot(df |> arrange(.data[[input$bar_var]]) |> head(10),
           aes(y=.data[[input$bar_var]], 
               x=Name, 
               fill = Manufacturer)) + 
      geom_bar(stat="identity", show.legend=T) +
      geom_hline(yintercept = summary[[input$bar_var]], linetype="dotted", color = "blue", size=1) +
      coord_flip()
  })
  
  # render scatterplot
  output$scatter_plot <- renderPlotly({
    plot_ly(data = df, x= df[[input$xaxis]], y = df[[input$yaxis]],
          type = "scatter",
          mode="markers") 
  })
  
  # render table
  output$table <- renderDT(
    df |> arrange(input$bar_var) |> head(10), options=list( info = FALSE, paging = F, searching = F), rownames = F
  )
  
  
  
}

shinyApp(ui, server)