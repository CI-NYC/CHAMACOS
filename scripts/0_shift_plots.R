library(patchwork)
library(tidyverse)

# scale <- function(x) {
#   rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
#   10 / rng
# }
# 
# original_scale <- readRDS(here::here("data/longitudinal_data_aligned.rds")) |>
#   select(c("op_kg_2_year_time_1",
#            "pyr_kg_2_year_time_1",
#            "carb_kg_2_year_time_1",
#            "neo_kg_2_year_time_1",
#            "mn_kg_2_year_time_1",
#            "gly_kg_2_year_time_1",
#            "paraq_kg_2_year_time_1")) |>
#   rename("organophosphates" = "op_kg_2_year_time_1",
#          "pyrethroids" = "pyr_kg_2_year_time_1",
#          "carbamates" = "carb_kg_2_year_time_1",
#          "neonicotinoids" = "neo_kg_2_year_time_1",
#          "manganese" = "mn_kg_2_year_time_1",
#          "glyphosates" = "gly_kg_2_year_time_1",
#          "paraquats" = "paraq_kg_2_year_time_1"
#   )
# 
# ten_unit_scale <- sapply(original_scale, scale)

data <- readRDS(here::here(paste0("data/observed_data.rds"))) |>
  select(c("newid",
           "op_kg_2_year_time_1",
           "pyr_kg_2_year_time_1",
           "carb_kg_2_year_time_1",
           "neo_kg_2_year_time_1",
           "mn_kg_2_year_time_1",
           "gly_kg_2_year_time_1",
           "paraq_kg_2_year_time_1")) |>
  rename("Organophosphates" = "op_kg_2_year_time_1",
         "Pyrethroids" = "pyr_kg_2_year_time_1",
         "Carbamates" = "carb_kg_2_year_time_1",
         "Neonicotinoids" = "neo_kg_2_year_time_1",
         "Manganese Fungicides" = "mn_kg_2_year_time_1",
         "Glyphosate" = "gly_kg_2_year_time_1",
         "Paraquat" = "paraq_kg_2_year_time_1"
  ) #|>
  # mutate(op_kg_2_year_time_1_mult = op_kg_2_year_time_1 * 0.8,
  #        pyr_kg_2_year_time_1_mult = pyr_kg_2_year_time_1 * 0.8,
  #        carb_kg_2_year_time_1_mult = carb_kg_2_year_time_1 * 0.8,
  #        neo_kg_2_year_time_1_mult = neo_kg_2_year_time_1 * 0.8,
  #        mn_kg_2_year_time_1_mult = mn_kg_2_year_time_1 * 0.8,
  #        gly_kg_2_year_time_1_mult = gly_kg_2_year_time_1 * 0.8,
  #        paraq_kg_2_year_time_1_mult = paraq_kg_2_year_time_1 * 0.8,
  #        op_kg_2_year_time_1_add = op_kg_2_year_time_1 + ten_unit_scale[1],
  #        pyr_kg_2_year_time_1_add = pyr_kg_2_year_time_1 + ten_unit_scale[2],
  #        carb_kg_2_year_time_1_add = carb_kg_2_year_time_1 + ten_unit_scale[3],
  #        neo_kg_2_year_time_1_add = neo_kg_2_year_time_1 + ten_unit_scale[4],
  #        mn_kg_2_year_time_1_add = mn_kg_2_year_time_1 + ten_unit_scale[5],
  #        gly_kg_2_year_time_1_add = gly_kg_2_year_time_1 + ten_unit_scale[6],
  #        paraq_kg_2_year_time_1_add = paraq_kg_2_year_time_1 + ten_unit_scale[7],
  #        )

data_mult <- data * 0.8
data_add <- data |>
  mutate(Organophosphates = Organophosphates + 0.1,
         Pyrethroids = Pyrethroids + 0.1,
         Carbamates = Carbamates + 0.1,
         Neonicotinoids = Neonicotinoids + 0.1,
         `Manganese Fungicides` = `Manganese Fungicides` + 0.1,
         Glyphosate = Glyphosate + 0.1,
         Paraquat = Paraquat + 0.1,
         )

plot_list <- list()
vars <- names(data |> select(-newid))

for (i in seq_along(vars)) {
  
  var <- vars[i]
  
  example_X <- max(data[[var]], na.rm = TRUE)
  example_X_mult <- example_X * 0.8
  
  df_plot <- data.frame(
    value = c(data[[var]], data_mult[[var]]),
    type = rep(c("Unshifted", "Shifted"), each = nrow(data))
  )
  
  p <- ggplot(df_plot, aes(x = value, fill = type)) +
    geom_density(alpha = 0.3, show.legend = FALSE) +
    geom_segment(x = example_X, xend = example_X_mult, y = 0.02, yend = 0.02,
                 arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +
    geom_point(x = example_X, y = 0.02, color = "red", size = 1) +
    geom_point(x = example_X_mult, y = 0.02, color = "blue", size = 1) +
    labs(title = var, x = "", y = "Density") +
    scale_fill_manual(values = c("Unshifted" = "red", "Shifted" = "blue")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  plot_list[[i]] <- p
  
  rm(var, example_X, example_X_add, df_plot, p)
}

combined_plot <- wrap_plots(plot_list, ncol = 2)
combined_plot

ggsave(plot = combined_plot, height = 7.04, width = 6, units = "in", filename = paste0("plots/shifts_plot_mult.pdf"))


plot_list <- list()
vars <- names(data |> select(-newid))

for (i in seq_along(vars)) {
  
  var <- vars[i]
  
  example_X <- max(data[[var]], na.rm = TRUE)
  example_X_add <- example_X + 0.1
  
  df_plot <- data.frame(
    value = c(data[[var]], data_add[[var]]),
    type = rep(c("Unshifted", "Shifted"), each = nrow(data))
  )
  
  p <- ggplot(df_plot, aes(x = value, fill = type)) +
    geom_density(alpha = 0.3, show.legend = FALSE) +
    geom_segment(x = example_X, xend = example_X_add, y = 0.02, yend = 0.02,
                 arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +
    geom_point(x = example_X, y = 0.02, color = "red", size = 1) +
    geom_point(x = example_X_add, y = 0.02, color = "blue", size = 1) +
    labs(title = var, x = "", y = "Density") +
    scale_fill_manual(values = c("Unshifted" = "red", "Shifted" = "blue")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  plot_list[[i]] <- p
  
  rm(var, example_X, example_X_add, df_plot, p)
}


combined_plot_additive <- wrap_plots(plot_list, ncol = 2)
combined_plot_additive

ggsave(plot = combined_plot_additive, height = 7.04, width = 6, units = "in", filename = paste0("plots/shifts_plot_add.pdf"))
