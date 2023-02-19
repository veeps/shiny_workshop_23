library(tidyverse)
library(magrittr)
library(janitor)
library(jsonlite)



df <- read_csv("data/education.csv") |>
  select(Year, contains("Total"), Education) |>
  rename_with(~str_remove(., 'Total')) |>
  clean_names() |>
  mutate_at(c(1:11), as.numeric)


df2 <- read_csv("data/education.csv") |>
  clean_names() |>
  select(year, contains("_male"), education) |>
  mutate_at(c(1:11), as.numeric) |>
  rename_with(~str_remove(., '_male'))

df3 <- read_csv("data/education.csv") |>
  clean_names() |>
  select(year, contains("_female"), education) |>
  mutate_at(c(1:11), as.numeric) |>
  rename_with(~str_remove(., '_female'))

college_race <- df |> filter(education == "College or more") |> select(-education) |>
  pivot_longer(cols= 2:11,
               names_to="demographic",
               values_to="total")

college_male <- df2 |> filter(education == "College or more") |> select(-education) |>
  pivot_longer(cols= 2:11,
               names_to="demographic",
               values_to="male")

college_female <- df3 |> filter(education == "College or more") |> select(-education) |>
  pivot_longer(cols= 2:11,
               names_to="demographic",
               values_to="female")

college <- college_race |> left_join(college_male) |> left_join(college_female)



ggplot(data = college |> filter(year > 2002), aes(x = year, y = male)) +
  geom_line() +
  facet_wrap(~demographic)

asian <- college |> filter(demographic == "asian")
