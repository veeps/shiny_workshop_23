library(DT)
library(readr)
library(dplyr)
library(plotly)

# Read in data ---------------------------------------------------------

df <- read.csv("data/cereal.csv")

# Task: Sort table by protein level in descending order
df |> arrange(desc()) |> head(10)

# Bar chart section -------------------------------------------------------

# render barchart
ggplot() + 
    geom_bar(stat="identity", fill = "#0add8c") +
    theme(axis.title.x=element_blank())


# Scatterplot section -------------------------------------------------------

# render scatterplot
ggplot() + geom_point() 

# create Plotly scatter
plot_ly() 
