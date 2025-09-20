#' Generate a 3-D Mobius in High Dimensions
#'
#' This function generates a dataset representing a structure with a mobius.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param noise_fun A function specifying which noise generation function to use for the additional dimensions. Default is \code{gen_noisedims}. Other options include \code{gen_wavydims1}, \code{gen_wavydims2}, and \code{gen_wavydims3}.
#' @param ... Additional arguments passed to the selected \code{noise_fun} (e.g., \code{m}, \code{s}, \code{theta}, \code{x1_vec}, \code{data}).
#' @return A data containing a mobius structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' mobius <- gen_mobius(n = 500, p = 4)
gen_mobius <- function(n = 500, p = 4, noise_fun = gen_noisedims, ...) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- geozoo::mobius(n = n, p = 3)$points |>
    tibble::as_tibble(.name_repair = "minimal")

  names(df) <- paste0("x", 1:3)

  if (p > 3) {
    # If set defaults
    if (identical(noise_fun, gen_noisedims)) {
      dots <- list(...)
      if (is.null(dots$m)) dots$m <- rep(0, p - 3)
      if (is.null(dots$s)) dots$s <- rep(0.05, p - 3)
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 3), dots))
    } else if (identical(noise_fun, gen_wavydims1)) {
      dots <- list(...)
      if (is.null(dots$theta)) dots$theta <- seq(pi / 6, 12 * pi / 6, length.out = n)
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 3), dots))
    } else if (identical(noise_fun, gen_wavydims2)) {
      dots <- list(...)
      if (is.null(dots$x1_vec)) dots$x1_vec <- df[, 1]
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 3), dots))
    } else if (identical(noise_fun, gen_wavydims3)) {
      dots <- list(...)
      if (is.null(dots$df)) dots$data <- df
      noise_df <- do.call(noise_fun, c(list(n = n, p = p - 3), dots))
    } else {
      noise_df <- noise_fun(n = n, p = p - 3, ...)
    }
    if (!is.matrix(noise_df)) noise_df <- as.matrix(noise_df)

    names(noise_df) <- paste0("x", 4:p)

    df <- dplyr::bind_cols(df, noise_df)

  }

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}
