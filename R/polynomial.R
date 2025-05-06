#' Generate Quadratic
#'
#' This function generates a dataset representing a structure with a quadratic pattern.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param range A numeric vector (default: c(-1, 1)) representing the range along x1 axis.
#' @return A data containing a quadratic structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' quadratic <- gen_quadratic(n = 500, p = 4)
gen_quadratic <- function(n = 500, p = 4, range = c(-1, 1)) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- matrix(0, nrow = n, ncol = 2)
  # gen the core curvilinear pattern in 2D
  df[, 1] <- stats::runif(n, range[1], range[2])
  poly_basis <- stats::poly(df[, 1], degree = 2, raw = TRUE)
  df[, 2] <- poly_basis[, 1] - poly_basis[, 2] + stats::runif(n, 0, 0.5)

  if (p > 2){

    noise_df <- gen_noisedims(n = n, p = (p-2), m = rep(0, p-2), s = rep(0.1, p-2)) |>
      as.matrix()
    colnames(noise_df) <- paste0("x", 3:p)

    df <- cbind(df, noise_df)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}


#' Generate Cubic
#'
#' This function generates a dataset representing a structure with a cubic pattern.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param range A numeric vector (default: c(-1, 2)) representing the range along x1 axis.
#' @return A data containing a cubic structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' cubic <- gen_cubic(n = 500, p = 4)
gen_cubic <- function(n = 500, p = 4, range = c(-1, 2)) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- matrix(0, nrow = n, ncol = 2)
  # gen the core curvilinear pattern in 2D
  df[, 1] <- stats::runif(n, range[1], range[2])
  poly_basis <- stats::poly(df[, 1], degree = 3, raw = TRUE)
  df[, 2] <- poly_basis[, 1] + poly_basis[, 2] - poly_basis[, 3] + stats::runif(n, 0, 0.5)

  if (p > 2){

    noise_df <- gen_noisedims(n = n, p = (p-2), m = rep(0, p-2), s = rep(0.1, p-2)) |>
      as.matrix()
    colnames(noise_df) <- paste0("x", 3:p)

    df <- cbind(df, noise_df)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}
