library(tidyverse)
library(fastDummies)

years <- "1"

data <- read_csv("data/rudolph_01b.csv")
  
data |> nrow()

data <- data |>
    filter(cancer_bl == 0) # filter out cancer, n = 608 remaning

data |> nrow() 
    
data <- data |>
    filter(!is.na(years_2y_10_5y)) # 445 observations non-missing
  
data |> nrow()

data <- data |>
    filter(years_2y_10_5y >= case_when(years == "1" ~ 1,
                                       years == "1_5" ~ 1.5,
                                       years == "2" ~ 2)) 

data |> nrow()

data <- data |>
    filter(d_op_kg_2_year_10_5y == 1,
           d_pyr_kg_2_year_10_5y == 1,
           d_carb_kg_2_year_10_5y == 1,
           d_neo_kg_2_year_10_5y == 1,
           d_mn_kg_2_year_10_5y == 1,
           d_gly_kg_2_year_10_5y == 1,
           d_paraq_kg_2_year_10_5y == 1)

data |> nrow()