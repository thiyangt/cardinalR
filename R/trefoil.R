#' Generate 4-D Trefoil Knot Coordinates
#'
#' This function generates coordinates for a 4-D trefoil knot.
#' The number of points is determined by the length of the theta and phi sequences.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param steps A numeric value (default: 5) representing the number of steps for the theta parameter.
#' @param noise_fun A function specifying which noise generation function to use for the additional dimensions. Default is \code{gen_noisedims}. Other options include \code{gen_wavydims1}, \code{gen_wavydims2}, and \code{gen_wavydims3}.
#' @param ... Additional arguments passed to the selected \code{noise_fun} (e.g., \code{m}, \code{s}, \code{theta}, \code{x1_vec}, \code{data}).
#' @return A data containing 4-D trefoil knot.
#' @export
#' @examples
#' set.seed(20240412)
#' trefoil4d <- gen_trefoil4d(n = 500, p = 4, steps = 5)
gen_trefoil4d <- function(n = 500, p = 4, steps = 5, noise_fun = gen_noisedims, ...) {

  if (p < 4) {
    cli::cli_abort("p should be greater than 4.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (steps <= 0) {
    cli::cli_abort("steps should be positive.")
  }

  # Ensure n is at least steps for a meaningful grid
  if (n < steps) {
    cli:: cli_alert_warning("`n` is less than `steps`. Adjusting `n` to `steps` for minimum resolution.")
    n <- steps
  }

  # Calculate the number of steps for phi based on the desired total points
  n_phi_steps <- ceiling(n / steps)

  # Define theta and phi sequences internally
  theta <- seq(0, by = 0.175, length.out = steps) # Keeps original theta density/range
  phi   <- seq(0, 4 * pi, length.out = n_phi_steps)

  # Create a tibble of all combinations of theta and phi
  df <- tidyr::expand_grid(theta = theta, phi = phi) |>
    # Calculate the 4D coordinates
    dplyr::mutate(
      x1 = cos(theta) * cos(phi),
      x2 = cos(theta) * sin(phi),
      x3 = sin(theta) * cos(1.5 * phi),
      x4 = sin(theta) * sin(1.5 * phi)
    ) |>
    # Reorder columns for clarity, putting inputs first
    dplyr::select(x1, x2, x3, x4)

  if (p > 4){

    # Use the selected noise function
    # If noise_fun is gen_noisedims and user didn't provide m or s, set defaults
    if (identical(noise_fun, gen_noisedims)) {
      dots <- list(...)
      if (is.null(dots$m)) dots$m <- rep(0, p - 4)
      if (is.null(dots$s)) dots$s <- rep(0.1, p - 4)
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 4), dots))
    } else {
      noise_df <- noise_fun(n = n, p = p - 4, ...)
    }
    if (!is.matrix(noise_df)) noise_df <- as.matrix(noise_df)

    colnames(noise_df) <- paste0("x", 5:p)

    df <- cbind(df, noise_df)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")

  return(df)
}


#' Generate 3-D Trefoil Knot Coordinates (Stereographic Projection)
#'
#' This function generates coordinates for a 3-D trefoil knot
#' by applying a stereographic projection from 4-D space.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param steps A numeric value (default: 5) representing the number of steps for the theta parameter.
#' @param noise_fun A function specifying which noise generation function to use for the additional dimensions. Default is \code{gen_noisedims}. Other options include \code{gen_wavydims1}, \code{gen_wavydims2}, and \code{gen_wavydims3}.
#' @param ... Additional arguments passed to the selected \code{noise_fun} (e.g., \code{m}, \code{s}, \code{theta}, \code{x1_vec}, \code{data}).
#' @return A data containing 3-D trefoil knot.
#' @export
#' @examples
#' set.seed(20240412)
#' trefoil3d <- gen_trefoil3d(n = 500, p = 4, steps = 5)
gen_trefoil3d <- function(n = 500, p = 4, steps = 5, noise_fun = gen_noisedims, ...) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (steps <= 0) {
    cli::cli_abort("steps should be positive.")
  }

  # First, get the 4D coordinates using the tibble-returning function
  df <- gen_trefoil4d(n = n, p = 4, steps = steps) |>
    # Filter out points where w is 1 to avoid division by zero
    dplyr::filter(x4 != 1) |>
    # Apply the stereographic projection
    dplyr::mutate(
      x1 = x1 / (1 - x4),
      x2 = x2 / (1 - x4),
      x3 = x3 / (1 - x4)
    ) |>
    # Select the 3D coordinates plus original parameters if desired
    dplyr::select(x1, x2, x3)

  if (p > 3){

    # Use the selected noise function
    # If noise_fun is gen_noisedims and user didn't provide m or s, set defaults
    if (identical(noise_fun, gen_noisedims)) {
      dots <- list(...)
      if (is.null(dots$m)) dots$m <- rep(0, p - 3)
      if (is.null(dots$s)) dots$s <- rep(0.1, p - 3)
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 3), dots))
    } else {
      noise_df <- noise_fun(n = n, p = p - 3, ...)
    }
    if (!is.matrix(noise_df)) noise_df <- as.matrix(noise_df)

    colnames(noise_df) <- paste0("x", 4:p)

    df <- cbind(df, noise_df)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")

  return(df)
}

utils::globalVariables(c("x1", "x2", "x3", "x4"))
