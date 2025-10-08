library(ggplot2)
library(tidyverse)
library(plotly)
library(sf)

data <- readRDS(here::here("data/observed_data.rds")) |>
  select(c("op_kg_2_year_time_1",
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
         "Paraquats" = "paraq_kg_2_year_time_1"
  )

data_sf <- st_as_sf(data, coords = c("Neonicotinoids", "Manganese Fungicides"))
hull_sf <- st_convex_hull(st_union(data_sf))

hull_indices <- chull(data$Neonicotinoids, data$`Manganese Fungicides`)  
hull_indices <- c(hull_indices, hull_indices[1])            
hull_data <- data[hull_indices, ] 

hull_data_closed <- rbind(hull_data, hull_data[1, ])

data_shifted = data |>
  mutate(Neonicotinoids = Neonicotinoids * 0.5)
data_shifted_sf <- st_as_sf(data_shifted, coords = c("Neonicotinoids", "Manganese Fungicides"))
inside_shifted <- st_within(data_shifted_sf, hull_sf, sparse = FALSE)[,1]

hull_shifted_sf <- st_convex_hull(st_union(data_shifted_sf))
ring <- st_difference(hull_shifted_sf, hull_sf)

data_outside <- data[!inside_shifted, ] |>
  filter(`Manganese Fungicides` > 0)
data_shifted_outside <- data_shifted[!inside_shifted, ] |>
  filter(`Manganese Fungicides` > 0)

p <- ggplot() +
  geom_sf(data = ring, fill = "gray40", alpha = 0.5, color = NA) +
  geom_polygon(data = hull_data_closed, aes(x = Neonicotinoids, y = `Manganese Fungicides`),
               fill = NA, color = "black", linetype = "dashed") +
  geom_segment(
    data = data_outside,
    aes(x = Neonicotinoids, y = `Manganese Fungicides`,
        xend = data_shifted_outside$Neonicotinoids,
        yend = data_shifted_outside$`Manganese Fungicides`),
    color = "black",
    linetype = "dashed",  
    size = 0.2,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed")
  ) +
  geom_point(data = data, aes(x = Neonicotinoids, y = `Manganese Fungicides`, color = "Observed"), size = 0.5) +
  geom_point(data = data_shifted, aes(x = Neonicotinoids, y = `Manganese Fungicides`, color = "Shifted"), size = 0.5) +
  scale_color_manual(values = c("Observed" = "black", "Shifted" = "red")) +
  
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.15),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.margin = margin(-2, -2, -2, -2)) +
  scale_x_continuous(breaks = seq(floor(min(data$Neonicotinoids)),
                                  ceiling(max(data$Neonicotinoids)), by = 0.1)) +
  scale_y_continuous(breaks = seq(floor(min(data$`Manganese Fungicides`)),
                                  ceiling(max(data$`Manganese Fungicides`)), by = 0.1))

p

ggsave(plot = p, height = 7.04, width = 6, units = "in", filename = paste0("plots/extrapolation.pdf"))
