library(sp)  

n <- 8
angle <- seq(0, 2*pi, length.out = n+1)[- (n+1)]
radius <- 0.4  
center <- c(0.5, 0.5)

octagon_x <- center[1] + radius * cos(angle)
octagon_y <- center[2] + radius * sin(angle)
octagon <- cbind(octagon_x, octagon_y)

x_left <- min(octagon_x)
y_range <- range(octagon_y[which(abs(octagon_x - x_left) < 1e-8)])
y_A <- mean(y_range)
point_A <- c(x_left + 0.02, y_A)  

lowest_y <- min(octagon_y)
point_B <- c(point_A[1] * 0.8, point_A[2] * 0.2)

closest_point_on_segment <- function(p, a, b) {
  ap <- p - a
  ab <- b - a
  t <- sum(ap * ab) / sum(ab * ab)
  t <- max(0, min(1, t))
  a + t * ab
}

distances <- numeric(n)
closest_points <- matrix(0, nrow=n, ncol=2)

for (i in 1:n) {
  a <- octagon[i,]
  b <- octagon[ifelse(i==n, 1, i+1),]
  proj <- closest_point_on_segment(point_B, a, b)
  dist <- sqrt(sum((proj - point_B)^2))
  distances[i] <- dist
  closest_points[i,] <- proj
}

order_idx <- order(distances)
point_C <- closest_points[order_idx[1],]
dist_C <- distances[order_idx[1]]
second_closest_dist <- distances[order_idx[2]]

cat("Coordinates:\n")
cat(sprintf("Point A: (%.3f, %.3f)\n", point_A[1], point_A[2]))
cat(sprintf("Point B: (%.3f, %.3f)\n", point_B[1], point_B[2]))
cat(sprintf("Point C: (%.3f, %.3f)\n\n", point_C[1], point_C[2]))

cat(sprintf("Distance B-C: %.4f\n", dist_C))
cat(sprintf("Distance B-second closest edge: %.4f\n", second_closest_dist))

plot(octagon, type='n', asp=1, xlim=c(0,1), ylim=c(0,1),
     main="Hypothetical case where X and Y are given a decrease shift but feasible shift causes X to increase",
     xlab = "X",
     ylab = "Y")
polygon(octagon, col='lightblue', border='blue')
points(rbind(point_A, point_B, point_C), pch=19, col=c("red", "green", "purple"))

label_point <- function(point, label, col) {
  text(point[1], point[2], label, pos=4, col=col, font=2)
  coord_text <- sprintf("(%.3f, %.3f)", point[1], point[2])
  text(point[1], point[2], coord_text, pos=2, col=col, cex=0.8)
}

label_point(point_A, "A", "red")
label_point(point_B, "B", "green")
label_point(point_C, "C", "purple")

segments(point_B[1], point_B[2], point_C[1], point_C[2], col='purple', lty=2)
