generate_simplex_points <- function(p, k) {
  # Generate k points in p-dimensional simplex
  # Sample k points from a (p-1)-dimensional Dirichlet distribution
  dirichlet_samples <- t(MASS::mvrnorm(n = k, mu = rep(0, p), Sigma = diag(p)))

  # Center the points to form a proper p-simplex
  simplex_points <- dirichlet_samples - rowMeans(dirichlet_samples)

  return(simplex_points)
}

# Approach 2: Adjusting the min-max range per dimension based on weights
# Assume we have weights for each dimension (lower weight for noisier dimensions)
dimension_weights <- c(1.0, 1.0, 0.5, 0.5)

data_list <- list(data1, data2, data3, data4)

n_features <- NCOL(data1)

global_min <- rep(Inf, n_features)
global_max <- rep(-Inf, n_features)

for (data in data_list) {
  current_min <- apply(data, 2, min)
  current_max <- apply(data, 2, max)
  global_min <- pmin(global_min, current_min)
  global_max <- pmax(global_max, current_max)
}


weighted_global_min_max_scale_r <- function(data, weights, global_min, global_max) {
  n_samples <- NROW(data)
  n_features <- NCOL(data)
  scaled_data <- matrix(0, nrow = n_samples, ncol = n_features)
  global_range <- global_max - global_min

  for (j in 1:n_features) {
    if (global_range[j] == 0) {
      scaled_data[, j] <- 0.5
    } else {
      min_val <- 0.5 - 0.5 * weights[j]
      max_val <- 0.5 + 0.5 * weights[j]
      scaled_data[, j] <- min_val + (data[, j] - global_min[j]) * (max_val - min_val) / global_range[j]
    }
  }
  return(scaled_data)
}

gen_wavydims <- function(n = 500, p = 4) {

  df[, 3] <- -sin(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)  # A sine-based curve
  df[, 4] <- cos(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)   # A cosine-based curve

}

gen_wavydims2 <- function(n = 500, p = 4) {

  df[, 3] <- theta + stats::rnorm(n, 0, 0.5)  # Simply map theta to the third dimension
  df[, 4] <- 2 * theta + stats::rnorm(n, 0, 0.5)  # Linear function for the fourth dimension

}

gen_wavydims3 <- function(n = 500, p = 4) {
  df[, 3] <- stats::runif(n, 0, h)     # Height along the cylinder
  df[, 4] <- a * sin(df[, 3])       # Curvy pattern in the 4th dimension
}

gen_wavydims4 <- function(n = 500, p = 4) {

  # Introduce non-linearity based on x1 and add random noise
  # You can experiment with different non-linear functions and noise levels
  power <- sample(2:5, 1) # Random power for the polynomial
  scale_factor <- stats::runif(1, 0.5, 2) # Random scaling
  noise_level <- stats::runif(1, 0, 0.05)

  df[, i] <- scale_factor * ((-1)^(i %/% 2)) * (df[, 1]^power) + stats::runif(n, -noise_level, noise_level * 2)

}
