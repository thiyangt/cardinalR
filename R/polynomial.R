#' Generate S-curve Data
#'
#' This function generates S-curve data, which is a commonly used dataset for
#' testing and visualizing dimensionality reduction algorithms.
#'
#' @param n The number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the generated S-curve data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' s_curve_data <- scurve(
#'   n = 100, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
gen_scurve <- function(n = 500, p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  noise_level <- 0.05
  scaling_factor <- 0.2

  a <- 3 * pi * stats::runif(n = n, min = -0.5, max = 0.5)
  x1 <- sin(a)
  x2 <- 2.0 * stats::runif(n = n)
  x3 <- sign(a) * (cos(a) - 1)

  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- x1
  coords[, 2] <- x2
  coords[, 3] <- x3

  if (p > 3) {
    for (i in 4:p) {
      # Strategy 1 & 2: Small variations around existing dimensions
      if (i == 4) coords[, i] <- x1 + scaling_factor * stats::runif(n, -noise_level, noise_level)
      if (i == 5) coords[, i] <- x2 + scaling_factor * stats::runif(n, -noise_level, noise_level)
      if (i == 6) coords[, i] <- x3 + scaling_factor * stats::runif(n, -noise_level, noise_level)
      # Strategy 3: Non-linear transformations with small scaling
      if (i > 6) {
        if (i %% 3 == 1) coords[, i] <- x1^2 * scaling_factor * noise_level + stats::runif(n, -noise_level * 0.5, noise_level * 0.5)
        if (i %% 3 == 2) coords[, i] <- x2 * x3 * scaling_factor * noise_level + stats::runif(n, -noise_level * 0.5, noise_level * 0.5)
        if (i %% 3 == 0) coords[, i] <- sin(x1 + x3) * scaling_factor * noise_level + stats::runif(n, -noise_level * 0.5, noise_level * 0.5)
      }
    }
  }

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate S-curve Data with a Hole
#'
#' This function generates S-curve data with a hole by filtering out samples that
#' are not close to a specified anchor point.
#'
#' @param n The number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the generated S-curve data with a hole.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' s_curve_hole_data <- gen_scurvehole(
#'   n = 100, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
gen_scurvehole <- function(n = 500, p = 4) {

  df <- gen_scurve(n = n, p = p) |>
    as.matrix()

  anchor <- c(0, 1, 0)

  if ((p %% 3) == 0) {

    anchor_vec <- rep(anchor, p/3)

  } else if ((p %% 3) == 1) {

    anchor_vec <- append(rep(anchor, round(p/3)), sample(anchor, 1))

  } else { #(p %% 3) == 2

    anchor_vec <- append(rep(anchor, round(p/3)), sample(anchor, 2))

  }

  indices <- rowSums((sweep(df, 2, anchor_vec, `-`))^2) > 0.3
  df <- df[indices, ]
  rownames(df) <- NULL

  df <- tibble::as_tibble(df, .name_repair = "minimal")

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

gen_swissroll <- function(n = 500, p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  t <- runif(n, min = 0, max = 3 * pi)  # Control parameter
  x1 <- t * cos(t)
  x2 <- t * sin(t)
  x3 <- runif(n, min = -1, max = 1)  # Adding some vertical variation

  coords <- matrix(0, nrow = n, ncol = p)
  coords[,1] <- x1
  coords[,2] <- x2
  if (p > 2) {
    coords[,3] <- x3
  }
  if (p > 3) {
    for (i in 4:p) {
      coords[,i] <- sin(i * t) / i  # Additional non-linearity
    }
  }

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
