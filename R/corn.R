# Function to gen a corn-shaped cluster in 4D space with an offset
gen_corn_blunted <- function(n = 500, p = 4, height = 5, base_radius = 1.5, tip_radius = 0.8) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (height <= 0) {
    cli::cli_abort("height should be positive.")
  }

  if (base_radius <= 0) {
    cli::cli_abort("base_radius should be positive.")
  }

  if (tip_radius <= 0) {
    cli::cli_abort("tip_radius should be positive.")
  }

  if (tip_radius >= base_radius) {
    cli::cli_abort("The tip_radius should be smaller than the base_radius of the corn.")
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
gen_corn_rectangular_base <- function(n = 500, p = 4, height = 5, base_width = c(3, 2), tip_radius = 0.5) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (height <= 0) {
    cli::cli_abort("height should be positive.")
  }

  if (any(base_width) <= 0) {
    cli::cli_abort("Values in the base_width should be positive.")
  }

  if (tip_radius <= 0) {
    cli::cli_abort("tip_radius should be positive.")
  }

  if (tip_radius >= any(base_width)) {
    cli::cli_abort("The tip_radius should be smaller than the base_width values of the corn.")
  }

  base_width_x <- base_width[1]
  base_width_y <- base_width[2]

  # gen points with a higher density near the tip
  height_values <- rexp(n, rate = 1 / (height / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, height)  # Cap heights to the maximum height

  # Base dimensions decrease linearly as height increases
  x_radii <- tip_radius + (base_width_x - tip_radius) * (height_values / height)
  y_radii <- tip_radius + (base_width_y - tip_radius) * (height_values / height)

  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- runif(n, -x_radii, x_radii)
  coords[, 2] <- runif(n, -y_radii, y_radii)
  coords[, 3] <- runif(n, -x_radii, x_radii)  # For the third dimension, using the same range as x

  # For the fourth dimension and beyond, taper toward the tip
  if (p > 3) {
    for (i in 4:p) {
      coords[, i - 1] <- runif(n, -0.1, 0.1) * (height - height_values) / height # Tapering
    }
  }

  # The last dimension is the height
  coords[, p] <- height_values

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

# Function to gen a corn-shaped cluster in 4D with a triangular base
gen_corn_triangular_base <- function(n = 500, p = 4, height = 5, base_width = 3, tip_radius = 0.5) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (height <= 0) {
    cli::cli_abort("height should be positive.")
  }

  if (base_width <= 0) {
    cli::cli_abort("base_width should be positive.")
  }

  if (tip_radius <= 0) {
    cli::cli_abort("tip_radius should be positive.")
  }

  if (tip_radius >= base_width) {
    cli::cli_abort("The tip_radius should be smaller than the base_width of the corn.")
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

  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- radii * (1 - u - v) # First triangle coordinate (mapped to x1)
  coords[, 2] <- radii * u         # Second triangle coordinate (mapped to x2)
  coords[, 3] <- radii * v         # Third triangle coordinate (mapped to x3)

  # For the fourth dimension and beyond, taper toward the tip
  if (p > 3) {
    for (i in 4:p) {
      coords[, i - 1] <- runif(n, -0.1, 0.1) * (height - height_values) / height # Tapering
    }
  }

  # The last dimension is the height
  coords[, p] <- height_values

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

# Function to gen a filled hexagonal pyramid in 4D space
gen_filled_hexagonal_pyramid <- function(n = 500, p = 4, height = 5, base_radius = 3) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (height <= 0) {
    cli::cli_abort("height should be positive.")
  }

  if (base_radius <= 0) {
    cli::cli_abort("base_radius should be positive.")
  }

  # Gen height values with more points near the base
  height_values <- runif(n, 0, height) # Uniformly distributed heights

  # The base radius decreases linearly as the height increases
  radii <- (base_radius * (height - height_values)) / height

  # Gen points within a hexagonal base in the first two dimensions
  hexagon_angles <- seq(0, 2 * pi, length.out = 7)[1:6] # 6 angles for hexagon

  # Randomly assign each point to a part of the hexagon
  selected_angles <- sample(hexagon_angles, n, replace = TRUE)

  # Gen points inside the hexagon (filling the base)
  radial_factors <- sqrt(runif(n, 0, 1)) # Ensures uniform distribution inside the hexagon
  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- radii * cos(selected_angles) * radial_factors
  coords[, 2] <- radii * sin(selected_angles) * radial_factors

  # For the third dimension and beyond, taper toward the tip
  if (p >= 3) {
    for (i in 3:p) {
      coords[, i] <- runif(n, -0.1, 0.1) * (height - height_values) / height # Tapering
    }
  }

  # The last dimension is the height
  coords[, p] <- height_values

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
