#' Generate Gaussian
#'
#' This function generates a dataset representing a structure with a Gaussian.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param m A numeric vector (default: c(0, 0, 0, 0)) specifying the mean for each cluster.
#' @param s A numeric matrix (default:  diag(4) * 0.01) representing the variance of along each dimension.
#' @return A data containing a Gaussian.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gaussian_data <- gen_gaussian(n = 500, p = 4, m = c(0, 0, 0, 0), s = diag(4))
gen_gaussian <- function(n = 500, p = 4, m = c(0, 0, 0, 0), s = diag(4) * 0.01) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (length(m) != p) {
    cli::cli_abort("m should contain exactly {.val {p}} values.")
  }

  if (!is.matrix(s)) {
    cli::cli_abort("s should be a matrix.")
  }

  if (NROW(s) != NCOL(s)) {
    cli::cli_abort("s should be a suqare matrix.")
  }

  if ((NROW(s) != p) | (NCOL(s) != p)) {
    cli::cli_abort("Number of rows and columns in s should be {.val {p}}.")
  }

  df <- mvtnorm::rmvnorm(n, mean = m, sigma = s)
  # Create the tibble
  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
