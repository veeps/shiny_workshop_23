library(tidyverse)
library(magrittr)
library(janitor)
library(plotly)

df <- read_csv("data/bachelors_degrees_conferred_usafacts 2.csv")

df2 <- pivot_longer(df, cols=2:152, names_to="year", values_to = "people") |>
  rename(race_ethnicity = Years) 

totals <- df2 |> 
  filter(year > 2005) |>
  filter(race_ethnicity== "Total") |>
  select(-race_ethnicity) |>
  rename(total = people)

bachelors <- df2 |>
  filter(year > 2005) |>
  filter(race_ethnicity != "Total") |>
  left_join(totals)  |>
  mutate(percent = round((people*100),1)) |>
  mutate(degree = "Bachelors") |>
  select(-people)

masters <- test |>
  mutate(degree = "Masters") |>
  mutate(race_ethnicity = str_remove_all(race_ethnicity, "\\(People\\)")) |>
  mutate(year=as.Date(year))

write_csv(masters, "masters_degree_conferred_usafacts_clean.csv")

plot_ly(
  data=masters,
  x=~year,
  y=~percent,
  color=~race_ethnicity,
  type='scatter',
  mode = 'lines'
)
