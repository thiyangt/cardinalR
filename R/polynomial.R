#' Generate Quadratic
#'
#' This function generates a dataset representing a structure with a quadratic pattern.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param range A numeric vector (default: c(-1, 1)) representing the range along x1 axis.
#' @param noise_fun A function specifying which noise generation function to use for the additional dimensions. Default is \code{gen_noisedims}. Other options include \code{gen_wavydims1}, \code{gen_wavydims2}, and \code{gen_wavydims3}.
#' @param ... Additional arguments passed to the selected \code{noise_fun} (e.g., \code{m}, \code{s}, \code{theta}, \code{x1_vec}, \code{data}).
#' @return A data containing a quadratic structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' quadratic <- gen_quadratic(n = 500, p = 4)
gen_quadratic <- function(n = 500, p = 4, range = c(-1, 1), noise_fun = gen_noisedims, ...) {

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

    # If set defaults
    if (identical(noise_fun, gen_noisedims)) {
      dots <- list(...)
      if (is.null(dots$m)) dots$m <- rep(0, p - 2)
      if (is.null(dots$s)) dots$s <- rep(0.05, p - 2)
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 2), dots))
    } else if (identical(noise_fun, gen_wavydims1)) {
      dots <- list(...)
      if (is.null(dots$theta)) dots$theta <- seq(pi / 6, 12 * pi / 6, length.out = n)
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 2), dots))
    } else if (identical(noise_fun, gen_wavydims2)) {
      dots <- list(...)
      if (is.null(dots$x1_vec)) dots$x1_vec <- df[, 1]
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 2), dots))
    } else if (identical(noise_fun, gen_wavydims3)) {
      dots <- list(...)
      if (is.null(dots$df)) dots$data <- df
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 2), dots))
    } else {
      noise_df <- noise_fun(n = n, p = p - 2, ...)
    }
    if (!is.matrix(noise_df)) noise_df <- as.matrix(noise_df)


    colnames(noise_df) <- paste0("x", 3:p)

    df <- cbind(df, noise_df)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}


#' Generate Cubic
#'
#' This function generates a dataset representing a structure with a cubic pattern.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param range A numeric vector (default: c(-1, 2)) representing the range along x1 axis.
#' @param noise_fun A function specifying which noise generation function to use for the additional dimensions. Default is \code{gen_noisedims}. Other options include \code{gen_wavydims1}, \code{gen_wavydims2}, and \code{gen_wavydims3}.
#' @param ... Additional arguments passed to the selected \code{noise_fun} (e.g., \code{m}, \code{s}, \code{theta}, \code{x1_vec}, \code{data}).
#' @return A data containing a cubic structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' cubic <- gen_cubic(n = 500, p = 4)
gen_cubic <- function(n = 500, p = 4, range = c(-1, 2), noise_fun = gen_noisedims, ...) {

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

    # If set defaults
    if (identical(noise_fun, gen_noisedims)) {
      dots <- list(...)
      if (is.null(dots$m)) dots$m <- rep(0, p - 2)
      if (is.null(dots$s)) dots$s <- rep(0.05, p - 2)
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 2), dots))
    } else if (identical(noise_fun, gen_wavydims1)) {
      dots <- list(...)
      if (is.null(dots$theta)) dots$theta <- seq(pi / 6, 12 * pi / 6, length.out = n)
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 2), dots))
    } else if (identical(noise_fun, gen_wavydims2)) {
      dots <- list(...)
      if (is.null(dots$x1_vec)) dots$x1_vec <- df[, 1]
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 2), dots))
    } else if (identical(noise_fun, gen_wavydims3)) {
      dots <- list(...)
      if (is.null(dots$df)) dots$data <- df
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 2), dots))
    } else {
      noise_df <- noise_fun(n = n, p = p - 2, ...)
    }
    if (!is.matrix(noise_df)) noise_df <- as.matrix(noise_df)


    df <- cbind(df, noise_df)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}
