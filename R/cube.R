#' Generate Cube with grid points
#'
#' This function generates a grid dataset with specified grid points along each axes.
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

  if (p <= 0) {
    cli::cli_abort("p should be positive.")
  }

  n_vec <- gen_nproduct(n = n, p = p)

  dims <- as.list(n_vec)
  df <- tidyr::expand_grid(!!!purrr::map(dims, seq_len))

  # Normalize each dimension to [0, 1] to form a proper cube
  df <- df |> dplyr::mutate(dplyr::across(dplyr::everything(),
                                          ~ (. - 1) / (n_vec[cur_column()] - 1)))

  # Create the tibble
  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}

#' Generate Cube with uniform points
#'
#' This function generates a grid dataset with specified uniform points along each axes.
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

  if (p <= 0) {
    cli::cli_abort("p should be positive.")
  }

  df <- matrix(NA, nrow = n, ncol = 3)
  half_length <- 1 / 2

  x_min <- -half_length
  x_max <- half_length

  # Generate n x d matrix of uniform points
  df <- matrix(stats::runif(n * p, min = x_min, max = x_max), nrow = n, ncol = p)

  # Create the tibble
  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)


}

#' Generate Cube with Hole
#'
#' This function generates a dataset representing a cube with a hole.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param r_hole A numeric value (default: 0.5) representing the radius of the hole.
#'
#' @return A data containing the cube data with a hole.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' cubehole <- gen_cubehole(n = 1000, p = 4)
gen_cubehole <- function(n = 500, p = 4, r_hole = 0.5) {

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
  plane_points <- plane_points |>
    dplyr::filter(distances > r_hole)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(plane_points)

}
