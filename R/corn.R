#' Generate Blunted Corn
#'
#' This function generates a dataset representing a blunted corn.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param h A numeric value (default: 5) representing the h of the corn.
#' @param rb A numeric value (default: 1.5) representing the base radius of the corn.
#' @param rt A numeric value (default: 0.8) representing the tip radius of the corn.
#'
#' @return A data containing the blunted corn.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' blunted_corn_data <- gen_corn_blunted(n = 500, p = 4, h = 5, rb = 1.5, rt = 0.8)
gen_corn_blunted <- function(n = 500, p = 4, h = 5, rb = 1.5, rt = 0.8) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (h <= 0) {
    cli::cli_abort("h should be positive.")
  }

  if (rb <= 0) {
    cli::cli_abort("rb should be positive.")
  }

  if (rt <= 0) {
    cli::cli_abort("rt should be positive.")
  }

  if (rt >= rb) {
    cli::cli_abort("The rt should be smaller than the rb of the corn.")
  }

  # Gen points with a higher density near the tip (along the last dimension - 'h')
  height_values <- stats::rexp(n, rate = 1 / (h / 2)) # Exponentially distributed heights
  height_values <- pmin(height_values, h)       # Cap heights to the maximum h

  # Generalized "radius" decreases linearly from the base to the tip
  radii <- rt + (rb - rt) * (height_values / h)

  # Generate generalized "angles" for the (p-1)-dimensional hypersphere
  angles <- matrix(runif(n * (p - 2), 0, 2 * pi), nrow = n)
  phi <- stats::runif(n, 0, pi) # One angle with range 0 to pi

  coords <- matrix(0, nrow = n, ncol = p)
  coords[, p] <- height_values # The last dimension is our 'h'

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

#' Generate Rectangular Based Corn
#'
#' This function generates a dataset representing a rectangular based corn.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param h A numeric value (default: 5) representing the h of the corn.
#' @param l_vec A numeric vector (default: c(3, 2)) representing the base lengths along the and y of the corn.
#' @param rt A numeric value (default: 0.5) representing the tip radius of the corn.
#'
#' @return A data containing the rectangular based corn.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' rectangular_corn_data <- gen_corn_rectangular_base(n = 500, p = 4, h = 5, l_vec = c(3, 2), rt = 0.5)
gen_corn_rectangular_base <- function(n = 500, p = 4, h = 5, l_vec = c(3, 2), rt = 0.5) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (h <= 0) {
    cli::cli_abort("h should be positive.")
  }

  if (any(l_vec <= 0)) {
    cli::cli_abort("Values in the base length vector should be positive.")
  }

  if (rt <= 0) {
    cli::cli_abort("rt should be positive.")
  }

  if (any(rt >= l_vec)) {
    cli::cli_abort("The rt should be smaller than the any base length values of the corn.")
  }

  base_width_x <- l_vec[1]
  base_width_y <- l_vec[2]

  # gen points with a higher density near the tip
  height_values <- rexp(n, rate = 1 / (h / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, h)  # Cap heights to the maximum h

  # Base dimensions decrease linearly as h increases
  x_radii <- rt + (base_width_x - rt) * (height_values / h)
  y_radii <- rt + (base_width_y - rt) * (height_values / h)

  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- runif(n, -x_radii, x_radii)
  coords[, 2] <- runif(n, -y_radii, y_radii)
  coords[, 3] <- runif(n, -x_radii, x_radii)  # For the third dimension, using the same range as x

  # For the fourth dimension and beyond, taper toward the tip
  if (p > 3) {
    for (i in 4:p) {
      coords[, i - 1] <- runif(n, -0.1, 0.1) * (h - height_values) / h # Tapering
    }
  }

  # The last dimension is the h
  coords[, p] <- height_values

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Triangular Based Corn
#'
#' This function generates a dataset representing a triangular based corn.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param h A numeric value (default: 5) representing the h of the corn.
#' @param l A numeric value (default: 3) representing the base length of the corn.
#' @param rt A numeric value (default: 0.5) representing the tip radius of the corn.
#'
#' @return A data containing the triangular based corn.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' triangular_corn_data <- gen_corn_triangular_base(n = 500, p = 4, h = 5, l = 3, rt = 0.5)
gen_corn_triangular_base <- function(n = 500, p = 4, h = 5, l = 3, rt = 0.5) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (h <= 0) {
    cli::cli_abort("h should be positive.")
  }

  if (l <= 0) {
    cli::cli_abort("The base length should be positive.")
  }

  if (rt <= 0) {
    cli::cli_abort("rt should be positive.")
  }

  if (rt >= l) {
    cli::cli_abort("The tip radius should be smaller than the base length of the corn.")
  }

  # gen points with a higher density near the tip
  height_values <- rexp(n, rate = 1 / (h / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, h)  # Cap heights to the maximum h

  # Base size decreases linearly as h increases
  radii <- rt + (l - rt) * (height_values / h)

  # gen points within a triangular cross-section at each h level
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
      coords[, i - 1] <- runif(n, -0.1, 0.1) * (h - height_values) / h # Tapering
    }
  }

  # The last dimension is the h
  coords[, p] <- height_values

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}


#' Generate Hexagonal Based Corn
#'
#' This function generates a dataset representing a hexagonal based corn.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param h A numeric value (default: 5) representing the h of the corn.
#' @param rb A numeric value (default: 3) representing the base radius of the corn.
#'
#' @return A data containing the hexagonal based corn.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' hexagonal_corn_data <- gen_filled_hexagonal_pyramid(n = 500, p = 4, h = 5, rb = 3)
gen_filled_hexagonal_pyramid <- function(n = 500, p = 4, h = 5, rb = 3) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (h <= 0) {
    cli::cli_abort("h should be positive.")
  }

  if (rb <= 0) {
    cli::cli_abort("rb should be positive.")
  }

  # Gen h values with more points near the base
  height_values <- runif(n, 0, h) # Uniformly distributed heights

  # The base radius decreases linearly as the h increases
  radii <- (rb * (h - height_values)) / h

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
      coords[, i] <- runif(n, -0.1, 0.1) * (h - height_values) / h # Tapering
    }
  }

  # The last dimension is the h
  coords[, p] <- height_values

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
