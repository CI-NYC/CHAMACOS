library(tidyverse)

R_statistic <- readRDS("data/R_statistic.rds")
R_statistic_first_5_shift <- readRDS("data/R_statistic_first_5_shift.rds")

for (i in 1:5)
{
  print(paste0("Time ", i))
  R_statistic[[i]] |> 
  select(R) |>
  summary() |>
  print()
}


for (i in 1:5)
{
  print(paste0("Time ", i))
  R_statistic_first_5_shift[[i]] |> 
    select(R) |>
    summary() |>
    print()
}


