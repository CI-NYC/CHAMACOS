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

read_results <- function(t, shift){
  data <- readRDS((paste0("results/mhtn_", shift, "_t_", t, "_shifting_all_20percent.rds")))
}

combined_results_list <- list()
contrast_results_list <- list()
results_shift <- list()
density_shift <- list()
combined_results_df <- data.frame()
for (z in c("obs", "mult"))
  
{
  results_t <- list()
  for (j in 1:5)
  {
    results_t[[j]] <- read_results(as.character(j), as.character(z)) 
    
    if(j > 1)
    {
      results_t[[j]]$estimate <- 1 - results_t[[j]]$estimate
      old_low <- results_t[[j]]$low
      results_t[[j]]$low <- 1 - results_t[[j]]$high
      results_t[[j]]$high <- 1 - old_low
    }
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

combined_results_df
contrasts_df

base_plot <- ggplot() +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) + 
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'dashed'),
        panel.grid.minor = element_blank())

results_plot <- base_plot +
  geom_point(data = combined_results_df, aes(x = factor(t), y = estimate, color = shift, group = shift, shape = shift),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(data = combined_results_df, 
                aes(x = factor(t), color = shift, group = shift,
                    ymin = conf.low, ymax = conf.high), width = 0.1, 
                position = position_dodge(width = 0.5)) + 
  labs(x = "", y = "Incidence of Chronic Hypertension", title = "d. Reducing ALL pesticides by 20%") +
  labs(color = "Treatment Regime",
       shape = "Treatment Regime") + 
  theme_minimal() + 
  scale_color_manual(values = c("#6495ED",  "#FF7F00")) +
  scale_shape_manual(values = c(9, 18)) +
  theme(
    plot.title = element_text(hjust = 0, size = 14),
    legend.position =  c(0.2, 0.85),
    legend.key.height = unit(0.8, "lines"),
    legend.key.width = unit(5, "lines"),
    legend.text = element_text(size = 9, margin = margin(r = 4)),
    legend.title = element_text(face = "bold", size = 9),
    legend.background = element_rect(fill = "white", color = "black", size = 0.25), 
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
    legend.spacing.y = unit(0.1, "cm"),
    plot.margin = unit(c(5.5, 5.5, 5.5, 9.5), "pt")
  )

contrast_plot <- ggplot(data = contrasts_df, aes(x = factor(t), y = estimate, color = contrast, group = contrast, shape = contrast)) +
  geom_point(position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, 
                position = position_dodge(width = 0.75)) +
  labs(x = "t", y = "Risk Difference (v. Observed)", title = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("#000000")) +
  scale_shape_manual(values = c(18)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) + 
  labs(color = "Regime Contrast") + 
  theme_minimal() +
  theme(
    legend.position =  "none",
    plot.margin = unit(c(5.5, 5.5, 5.5, 9.5), "pt")
  ) +
  ylim(-0.12, 0.03)


plots_combined <- ggarrange(results_plot, 
                            contrast_plot,
                            labels = c("A", "B"),
                            align = "h",
                            nrow = 2)

ggsave(plot = plots_combined, filename = here::here("plots/reducing_all_exposures.pdf"),
       width = 12, height = 9, dpi = 300, units = "in", device = pdf)

contrasts_df <- contrasts_df |>
  select(t, contrast, estimate, std.error, conf.low, conf.high)

combined_results_df <- combined_results_df |>
  select(t, shift, estimate, std.error, conf.low, conf.high)

write.csv(combined_results_df, here::here("results_csv/results_all_exposures.csv"))
write.csv(contrasts_df, here::here("results_csv/contrasts_all_exposures.csv"))

