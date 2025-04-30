#' Generate Cube with grid points
#'
#' This function generates a grid dataset with specified grid points along each axes..
#'
#' @param n A numeric vector (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the cube with grid points.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gridcube <- gen_gridcube(n = 500, p = 4)
gen_gridcube <- function(n = 500, p = 4) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  n_vec <- gen_nproduct(n = n, p = p)

  dims <- as.list(n_vec)
  df <- tidyr::expand_grid(!!!purrr::map(dims, seq_len))

  # Create the tibble
  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate Cube with uniform points
#'
#' This function generates a grid dataset with specified uniform points along each axes..
#'
#' @param n A numeric vector (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the cube with uniform points.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' unifcube <- gen_unifcube(n = 500, p = 4)
gen_unifcube <- function(n = 500, p = 4) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- matrix(NA, nrow = n, ncol = 3)
  half_length <- 1 / 2
  x_min <- -half_length
  x_max <- half_length
  y_min <- -half_length
  y_max <- half_length
  z_min <- - half_length
  z_max <-  half_length

  for (i in 1:n) {
    x1 <- runif(1, x_min, x_max)
    x2 <- runif(1, y_min, y_max)
    x3 <- runif(1, z_min, z_max)
    df[i, ] <- c(x1, x2, x3)
  }

  if (p > 3) {
    noise_df <- gen_noisedims(n = NROW(df), p = (p-3), m = rep(0, p-3), s = rep(0.05, p-3)) |>
      as.matrix()
    colnames(noise_df) <- paste0("x", 4:p)

    df <- cbind(df, noise_df)
  }

  # Create the tibble
  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)


}

#' Generate Cube with Hole
#'
#' This function generates a dataset representing a cube with a hole.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the cube data with a hole.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' cubehole <- gen_cubehole(n = 1000, p = 4)
gen_cubehole <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  plane_points <- gen_unifcube(n = n, p = p)

  # Compute Euclidean distance from the center
  distances <- sqrt(rowSums(plane_points^2))

  # Remove points inside the hole
  hole_radius <- 0.5
  plane_points <- plane_points |>
    dplyr::filter(distances > hole_radius)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(plane_points)

}
