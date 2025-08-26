library(tidyverse)
library(lmtp)
library(ggpubr)
library(gridExtra)
library(grid)
library(scales)

tidy.lmtp_survival <- function(x, ...) {
  out <- do.call("rbind", lapply(x, tidy))
  out$t <- 1:length(x)
  out[, c(ncol(out), 1:ncol(out) - 1)]
}

isotonic_projection <- function(x, alpha = 0.05) {
  cv <- abs(qnorm(p = alpha / 2))
  estim <- tidy.lmtp_survival(x)
  iso_fit <- isotone::gpava(1:length(x), estim$estimate)
  for (i in seq_along(x)) {
    x[[i]]$theta <- iso_fit$x[i]
    x[[i]]$low <- (x[[i]]$theta - (qnorm(0.975) * x[[i]]$standard_error))
    x[[i]]$high <- (x[[i]]$theta + (qnorm(0.975) * x[[i]]$standard_error))
  }
  x
}

combined_results_df_final <- tibble(
  estimate = numeric(0),
  std.error = numeric(0),
  conf.low = numeric(0),
  conf.high = numeric(0), 
  shift = numeric(0),
  t = numeric(0)
)

contrasts_df_final <- tibble(
  shift = numeric(0),
  ref = numeric(0),
  estimate = numeric(0),
  std.error = numeric(0),
  conf.low = numeric(0),
  conf.high = numeric(0),
  p.value = numeric(0),
  t = numeric(0),
  contrast = numeric(0)
)

for (s in c("all", "first_5", "last_2", "paraq"))
{

read_results <- function(shift){
  if (shift == "mult")
  {
    data <- readRDS((paste0("results/mhtn_", shift, "_shifting_", s, "_20percent_single.rds")))
  } else if (shift == "obs")
  {
    data <- readRDS("results/mhtn_obs_shifting_all_20percent_single.rds")
  }
}


combined_results_list <- list()
contrast_results_list <- list()
results_shift <- list()
density_shift <- list()
combined_results_df <- data.frame()
for (z in c("obs", "mult"))
  
{
  results_t <- list()
  for (j in 1:1)
  {
    results_t[[j]] <- read_results(as.character(z)) 
  }
  
  results_shift[[z]] <- isotonic_projection(results_t)
}

tidied_results <- map(results_shift, ~ map_dfr(.x, tidy))

dfobs <- tidied_results[[1]] |>
  mutate(shift = "obs",
         t = row_number())

dfshift <- tidied_results[[2]]|>
  mutate(shift = "mult", 
         t = row_number())

combined_results_df <- dfobs |>
  merge(dfshift, all = TRUE) |>
  distinct() |>
  arrange(t, shift)

contrast_shift_obs <- map2(results_shift[[2]], results_shift[[1]], ~lmtp_contrast(.x, ref = .y))

combined_vals_contrast_shift_obs <- map_dfr(contrast_shift_obs, ~ {
  data.frame(vals = .x$estimates)  
}) |>
  mutate(t = row_number())

contrasts_df <- combined_vals_contrast_shift_obs |>
  mutate(contrast = "shift v. obs") |>
  arrange(t, contrast)

colnames(contrasts_df) <- gsub("^vals\\.", "", colnames(contrasts_df))

combined_results_df <- combined_results_df |>
  mutate(type = s)

combined_results_df_final <- rbind(combined_results_df_final, combined_results_df)
  
contrasts_df <- contrasts_df |>
  mutate(type = s)

contrasts_df_final <- rbind(contrasts_df_final, contrasts_df)
}

contrasts_df_final <- contrasts_df_final |>
  mutate(type = case_when(type == "all" ~ "All Pesticides",
                          type == "first_5" ~ "First 5 Pesticides",
                          type == "last_2" ~ "Last 2 Pesticides",
                          type == "paraq" ~ "Paraquat Only"
                          ))

contrast_plot <- ggplot(data = contrasts_df_final, aes(x = factor(type), y = estimate, color = type, group = type, shape = type)) +
  geom_point(position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, 
                position = position_dodge(width = 0.75)) +
  labs(x = "", y = "Risk Difference (v. Observed)", title = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(color = "Reduction", shape = "Reduction") + 
  theme_minimal() +
  theme(
    legend.position =  "none",
    plot.margin = unit(c(5.5, 5.5, 5.5, 9.5), "pt")
  )

ggsave(plot = contrast_plot, filename = here::here(paste0("plots/reducing_exposures_single_timepoint.pdf")),
       width = 12, height = 9, dpi = 300, units = "in", device = pdf)

write.csv(contrasts_df_final, here::here(paste0("results_csv/contrasts_single_timepoint.csv")))