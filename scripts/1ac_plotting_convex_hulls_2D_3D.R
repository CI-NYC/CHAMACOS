library(tidyverse)
library(geometry)
library(rgl)
library(htmlwidgets)


pol <- readRDS("data/pol_time_1.rds") |>
  rename("organophosphates" = "op_kg_2_year_time_1",
         "pyrethroids" = "pyr_kg_2_year_time_1",
         "carbamates" = "carb_kg_2_year_time_1",
         "neonicotinoids" = "neo_kg_2_year_time_1",
         "maganese" = "mn_kg_2_year_time_1",
         "glyphosates" = "gly_kg_2_year_time_1",
         "paraquats" = "paraq_kg_2_year_time_1"
  )

## 2D Plots

combs <- combn(1:7, 2)

par(mfrow = c(5, 5)) 

for (i in 1:ncol(combs)) {
  col1 <- combs[1, i]
  col2 <- combs[2, i]
  
  x <- pol[, col1]
  y <- pol[, col2]
  
  col1_name <- colnames(pol)[col1]
  col2_name <- colnames(pol)[col2]
  
  points <- cbind(x, y)
  
  hull_indices <- chull(points)
  hull_indices <- c(hull_indices, hull_indices[1])
  
  plot(points, main = paste(""), xlab = paste(col1_name), ylab = paste(col2_name), pch = 19)
  polygon(points[chull(points), ], border = "red")
}

## 3D Plots

combs <- combn(1:7, 3)

for (i in 1:ncol(combs)) {
  cols <- combs[, i]
  data3d <- pol[, cols]
  
  hull <- tryCatch({
    convhulln(data3d)
  }, error = function(e) {
    message(paste("Skipping combination", paste(cols, collapse = ","), ": convex hull failed"))
    return(NULL)
  })
  
  if (!is.null(hull)) {
    open3d()
    points3d(data3d, col = "blue", size = 5)
    
    for (j in 1:nrow(hull)) {
      triangles3d(data3d[hull[j, ], ], color = "skyblue", alpha = 0.4)
    }
    
    title3d(
      main = paste("Convex Hull"),
      xlab = colnames(pol)[cols[1]],
      ylab = colnames(pol)[cols[2]],
      zlab = colnames(pol)[cols[3]]
    )
    
    ticks <- seq(0, 1, by = 0.2)
    labels <- sprintf("%.1f", ticks)
    
    axis3d("x", at = ticks, labels = labels)
    axis3d("y", at = ticks, labels = labels)
    axis3d("z", at = ticks, labels = labels)
  }
}

## EXAMPLE

for (i in 20) {
  cols <- combs[, i]
  data3d <- pol[, cols]
  
  hull <- tryCatch({
    convhulln(data3d)
  }, error = function(e) {
    message(paste("Skipping combination", paste(cols, collapse = ","), ": convex hull failed"))
    return(NULL)
  })
  
  if (!is.null(hull)) {
    open3d()
    points3d(data3d, col = "blue", size = 5)
    
    for (j in 1:nrow(hull)) {
      triangles3d(data3d[hull[j, ], ], color = "skyblue", alpha = 0.4)
    }
    
    title3d(
      main = paste("Convex Hull"),
      xlab = colnames(pol)[cols[1]],
      ylab = colnames(pol)[cols[2]],
      zlab = colnames(pol)[cols[3]]
    )
    
    ticks <- seq(0, 1, by = 0.2)
    labels <- sprintf("%.1f", ticks)
    
    axis3d("x", at = ticks, labels = labels)
    axis3d("y", at = ticks, labels = labels)
    axis3d("z", at = ticks, labels = labels) |>
      saveWidget("myplot.html")
  }
}

rglwidget() |>
  saveWidget("plots/3dsupport.html")


