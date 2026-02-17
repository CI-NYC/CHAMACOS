#remotes::install_github("nt-williams/lmtp@mlr3superlearner")
library(lmtp)
library(tidyverse)
library(ife)

### How to test if there is an interactive effect between two exposures?

## Read in lmtp objects

## Joint reduction
A6A7 <- readRDS(here::here("results/mhtn_mult_shifting_last_2_20percent_single.rds"))

## Single reduction on A6
A6 <- readRDS(here::here("results/mhtn_mult_shifting_gly_20percent_single.rds"))

## Single reduction on A7
A7 <- readRDS(here::here("results/mhtn_mult_shifting_paraq_20percent_single.rds"))

## Observed (no intervention)
OBS <- readRDS(here::here("results/mhtn_obs_shifting_all_20percent_single.rds"))

# We can combine the estimates and eif values -> if this crosses 0, then we fail to reject the null hypothesis that there is no interactive effect
A6A7$estimate - A6$estimate - A7$estimate + OBS$estimate

#Estimate: 0.004
#Std. error: 0.006
#95% Conf. int.: -0.008, 0.017

# We find that the confidence interval crosses the null. Therefore, we fail to reject the null hypothesis that there is no interactive effect
