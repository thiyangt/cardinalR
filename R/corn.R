# Function to gen a corn-shaped cluster in 4D space with an offset
gen_blunted_corn <- function(n = 500, height = 5, base_radius = 1.5, tip_radius = 0.8) {

  if (p < 2) {
    stop(cli::cli_alert_danger("p should be 2 or greater."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  # Gen points with a higher density near the tip (along the last dimension - 'height')
  height_values <- stats::rexp(n, rate = 1 / (height / 2)) # Exponentially distributed heights
  height_values <- pmin(height_values, height)       # Cap heights to the maximum height

  # Generalized "radius" decreases linearly from the base to the tip
  radii <- tip_radius + (base_radius - tip_radius) * (height_values / height)

  # Generate generalized "angles" for the (p-1)-dimensional hypersphere
  angles <- matrix(runif(n * (p - 2), 0, 2 * pi), nrow = n)
  phi <- stats::runif(n, 0, pi) # One angle with range 0 to pi

  coords <- matrix(0, nrow = n, ncol = p)
  coords[, p] <- height_values # The last dimension is our 'height'

  # Convert hyperspherical coordinates to Cartesian-like coordinates
  if (p == 2) {
    coords[, 1] <- radii * cos(phi) # Using phi as the angle in 2D
    coords[, 2] <- height_values
  } else if (p == 3) {
    coords[, 1] <- radii * cos(angles[, 1]) * sin(phi)
    coords[, 2] <- radii * sin(angles[, 1]) * sin(phi)
    coords[, 3] <- radii * cos(phi)
    coords[, 4] <- height_values
  } else if (p > 3) {
    coords[, 1] <- radii * cos(angles[, 1]) * sin(phi)
    coords[, 2] <- radii * sin(angles[, 1]) * sin(phi)
    coords[, 3] <- radii * cos(phi)
    for (i in 4:p) {
      product_of_sines <- 1
      for (j in 1:(i - 2)) {
        product_of_sines <- product_of_sines * sin(angles[, j])
      }
      coords[, i - 1] <- radii * product_of_sines * cos(ifelse(i == p, phi, angles[, i - 2]))
      if (i < p) {
        coords[, i] <- radii * product_of_sines * sin(angles[, i - 2])
      }
    }
    coords[, p] <- height_values
  }

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

# Function to gen a corn-shaped cluster in 4D with a rectangular base
gen_corn_cluster_rectangular_base <- function(n, height = 5, base_width_x = 3, base_width_y = 2, tip_radius = 0.5) {
  if (p < 2) {
    stop(cli::cli_alert_danger("p should be 2 or greater."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  # gen points with a higher density near the tip
  height_values <- rexp(n, rate = 1 / (height / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, height)  # Cap heights to the maximum height

  # Base dimensions decrease linearly as height increases
  x_radii <- tip_radius + (base_width_x - tip_radius) * (height_values / height)
  y_radii <- tip_radius + (base_width_y - tip_radius) * (height_values / height)

  # gen points uniformly within the rectangular cross-section at each height level
  x1 <- runif(n, -x_radii, x_radii)
  x2 <- runif(n, -y_radii, y_radii)
  x3 <- runif(n, -x_radii, x_radii)  # For the third dimension, using the same range as x
  x4 <- height_values  # Fourth dimension is the height

  df <- tibble::tibble(
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4
  )

  df <- df |>
    sweep(2, offset, "+") |>
    tibble::as_tibble()

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

# Function to gen a corn-shaped cluster in 4D with a triangular base
gen_corn_cluster_triangular_base_4d <- function(n, height = 5, base_width = 3, tip_radius = 0.5) {
  if (n < 0) {
    stop(cli::cli_alert_danger("n should be positive."))
  }

  # gen points with a higher density near the tip
  height_values <- rexp(n, rate = 1 / (height / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, height)  # Cap heights to the maximum height

  # Base size decreases linearly as height increases
  radii <- tip_radius + (base_width - tip_radius) * (height_values / height)

  # gen points within a triangular cross-section at each height level
  # Using barycentric coordinates to gen points inside a triangle
  u <- runif(n)
  v <- runif(n)
  is_outside <- (u + v) > 1
  u[is_outside] <- 1 - u[is_outside]
  v[is_outside] <- 1 - v[is_outside]

  # Scale the triangular points by the radius (for tapering)
  x1 <- radii * (1 - u - v)  # First triangle coordinate
  x2 <- radii * u            # Second triangle coordinate
  x3 <- radii * v            # Third triangle coordinate
  x4 <- height_values        # Fourth dimension (height)

  df <- tibble::tibble(
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4
  )

  df <- df |>
    sweep(2, offset, "+") |>
    tibble::as_tibble()

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

# Function to gen a filled hexagonal pyramid in 4D space
gen_filled_hexagonal_pyramid_4d <- function(n, height = 5, base_radius = 3) {
  if (n < 0) {
    stop(cli::cli_alert_danger("n should be positive."))
  }

  # gen height values with more points near the base
  height_values <- runif(n, 0, height)  # Uniformly distributed heights

  # The base radius decreases linearly as the height increases
  radii <- (base_radius * (height - height_values)) / height

  # gen points within a hexagonal base in the x, y plane
  angles <- runif(n, 0, 2 * pi)
  hexagon_angles <- seq(0, 2 * pi, length.out = 7)[1:6]  # 6 angles for hexagon

  # Randomly assign each point to a part of the hexagon
  selected_angles <- sample(hexagon_angles, n, replace = TRUE)

  # gen points inside the hexagon (filling the base)
  radial_factors1 <- sqrt(runif(n, 0, 1))  # Ensures uniform distribution inside the hexagon
  radial_factors12 <- sqrt(runif(n, 0, 1))  # Ensures uniform distribution inside the hexagon

  x1 <- radii * cos(selected_angles) * radial_factors1
  x2 <- radii * sin(selected_angles) * radial_factors2

  # For the third dimension (z), gen points inside the volume, tapering toward the tip
  x3 <- runif(n, -0.1, 0.1) * (height - height_values) / height  # Tapering with small variance

  # The fourth dimension (w) is the height
  x4 <- height_values

  df <- tibble::tibble(
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4
  )

  df <- df |>
    sweep(2, offset, "+") |>
    tibble::as_tibble()

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
