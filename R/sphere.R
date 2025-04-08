#' Generate Circle in p-d
#'
#' This function generates a dataset representing a structure with a circle.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param shift A numeric vector (default: c(0, 0)) representing the shift along x1, x2 respectively.
#' @param scale_fac A numeric vector (default:  c(1, 1)) representing the scale factors along x1, x2 respectively.
#' @return A data containing a circle.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' circle_data <- gen_circle(n = 500, p = 4)
gen_circle <- function(n = 500, p = 3, shift = c(0, 0), scale_fac = c(1, 1)){

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  theta <- stats::runif(n, 0.0, 2 * pi)
  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- scale_fac[1] * (shift[1] + cos(theta))
  coords[, 2] <- scale_fac[2] * (shift[2] + sin(theta))

  # Introduce scaling factors for subsequent dimensions
  scaling_factors <- sqrt(cumprod(c(1, rep(0.5, p - 2)))) # Example: decreasing scale

  if (p > 2) {
    # Apply remaining dimensions with sinusoidal patterns
    for (i in 3:p) {
      # Introduce a phase shift for each dimension to make them distinct
      phase_shift <- (i - 2) * (pi / (2 * p))
      coords[, i] <- scaling_factors[i-1] * sin(theta + phase_shift)
    }
  }

  df <- suppressMessages(tibble::as_tibble(coords, .name_repair = "unique"))
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Curvy Cell Cycle in p-d
#'
#' This function generates a dataset representing a structure with a curvy cell cycle.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param r A numeric value (default: sqrt(3) / 3) representing the radius factor along x2.
#' @return A data containing a curvy cell cycle.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' curvy_cycle_data <- gen_curvycycle(n = 500, p = 4, shift = c(0, sqrt(3) / 3, 0), scale_fac = c(1, 1, 1/3))
gen_curvycycle <- function(n = 500, p = 4, shift = c(0, sqrt(3) / 3, 0), scale_fac = c(1, 1, 1/3)){

  if (p <= 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  theta <- stats::runif(n, 0.0, 2 * pi)
  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- scale_fac[1] * (shift[1] + cos(theta))
  coords[, 2] <- scale_fac[2] * (shift[2] + sin(theta))
  coords[, 3] <- scale_fac[3] * (shift[3] + cos(3 * theta))

  # Introduce scaling factors for subsequent dimensions
  scaling_factors <- sqrt(cumprod(c(1, rep(0.5, p - 3)))) # Example: decreasing scale

  if (p > 3) {
    # Apply remaining dimensions with sinusoidal patterns
    for (i in 4:p) {
      # Introduce a phase shift for each dimension to make them distinct
      phase_shift <- (i - 2) * (pi / (2 * p))
      coords[, i] <- scaling_factors[i-2] * sin(theta + phase_shift)
    }
  }

  df <- suppressMessages(tibble::as_tibble(coords, .name_repair = "unique"))
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
