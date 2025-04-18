#' Generate Swiss Roll Data
#'
#' This function generates swiss roll data.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param w A numeric vector (default: c(-1, 1)) representing the vertical variation.
#' @return A data containing the generated swiss roll data.
#' @references
#' Agrafiotis, D. K., & Xu, H. (2002).
#' A self-organizing principle for learning nonlinear manifolds.
#' \emph{Proceedings of the National Academy of Sciences}, \emph{99}(25), 15869-15872.
#'
#'
#' Roweis, S. T., & Saul, L. K. (2000).
#' Nonlinear dimensionality reduction by locally linear embedding.
#' \emph{Science}, \emph{290}(5500), 2323-2326.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_swissroll(n = 500, p = 4)
#' head(data, 5)
gen_swissroll <- function(n = 500, p = 4, w = c(-1, 1)) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  t <- stats::runif(n, min = 0, max = 3 * pi)  # Control parameter
  x1 <- t * cos(t)
  x2 <- t * sin(t)
  x3 <- stats::runif(n, min = w[1], max = w[2])  # Adding some vertical variation

  df <- matrix(0, nrow = n, ncol = 3)
  df[,1] <- x1
  df[,2] <- x2
  df[,3] <- x3

  if (p > 3) {
    noise_df <- gen_noisedims(n = n, p = (p-3), m = rep(0, p-3), s = rep(0.05, p-3)) |>
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
