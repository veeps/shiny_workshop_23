library(shiny)
library(DT)
library(readr)
library(dplyr)
library(plotly)

# read in data 
df <- read.csv("data/cereal_2.csv") |> select(-Manufacturer)
nutrient_dense <- c( "Carbs", "Fiber",  "Potassium","Protein", "Vitamins")


ui <- fluidPage(
  
  # javascrit
  tags$script(src = "@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
  
  # header
  
  fluidRow(style="padding:40px; background: #03bf7b; color: #ffffff",
           tags$i(class = "fa-solid fa-bowl-spoon"),
           h1("Cereal Selector.. Aka, Adulting is Hard :( ", align="center")
  ), # end fluid row
  
  fluidRow(style="padding:40px; background: #f2f2f2",
           h3("Nutritional Density ", align="center"),
           p("Let's start by ranking cereals based on their nutritional density. Which ones have more of the 'good stuff'? ", align="center")
  ), # end fluid row
  
  # Bar chart section -------------------------------------------------------
  
  # create side panel with dropdown menu
  fluidRow(style="padding:40px; ",
           column(3,radioButtons(inputId="bar_var", #references the input to server
                                 label = h3("Select Variable"), # text that appears on UI
                                 choices=nutrient_dense |> sort(),
                                 selected="Protein")),
           # plot bar chart
           column(9,plotOutput("bar_plot"))
  ),# end fluidRow
  
  # Data table section ----------------------------------------------------
  
  fluidRow(style="padding-left:40px; padding-right:40px; padding-bottom:40px",
           DTOutput("table")
  ),
  
  # Scatter plot section --------------------------------------------------
  
  # create side panel with radio buttons
  fluidRow(style="padding:40px; background: #f2f2f2",
           column(3,div(selectInput(inputId="xaxis", #references the input to server
                                    label = h3("Select X Variable"), # text that appears on UI
                                    choices=colnames(df)[3:7] |> sort(),
                                    selected="Calories"),
                        selectInput(inputId="yaxis", #references the input to server
                                    label = h3("Select Y Variable"), # text that appears on UI
                                    choices=colnames(df)[3:11] |> sort(),
                                    selected="Sugars")
           ) #end div
           ), # end column
           # plot bar chart
           column(9,plotlyOutput("scatter_plot"))
  )# end fluidRow
  
  
  
  
  
) # end UI

server <- function(input, output, session) {
  
  
  # create summary table of median values
  summary <- df |>
    summarise_if(is.numeric, median, na.rm = TRUE)
  
  nutrient_df <- df |> select(Name, all_of(nutrient_dense))
  
  # create reactive dataframe subset
  df_sub <- reactive({
    df  |> arrange(input$bar_var) |> head(10)
  })
  
  # render barchart
  output$bar_plot <- renderPlot({
    ggplot(df_sub(),
           aes(y=.data[[input$bar_var]], 
               x=Name)) + 
      geom_bar(stat="identity", fill = "#0add8c") +
      geom_vline(xintercept = summary[[input$bar_var]], linetype="dotted", color = "#fe788a", size=1) +
      theme(axis.title.y=element_blank()) +
      scale_x_discrete(labels=function(x){gsub(" ", "\n", df_sub()$Name)})
  })
  
  
  # render table
  output$table <- renderDT(
    df_sub(), options=list( info = FALSE, paging = T, searching = F, pageLength = 5), rownames = F
  )
  
  
  # render scatterplot
  output$scatter_plot <- renderPlotly({
    plot_ly(data = df, x= df[[input$xaxis]], y = df[[input$yaxis]],
            type = "scatter",
            mode= "markers",
            hovertemplate = paste0(
              df$Name, "<br>", input$xaxis, ": %{x}<br>", input$yaxis, ": %{y}<extra></extra>")) |>
      layout(
        xaxis=list(title = input$xaxis),
        yaxis=list(title = input$yaxis)
      )
    
  })
  
  
}

shinyApp(ui, server)