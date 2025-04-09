#' Generate Cube with grid points
#'
#' This function generates a grid dataset with specified grid points along each axes..
#'
#' @param n A numeric vector (default: c(10, 10)) representing the number of grid points along each axes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the cube with grid points.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' one_grid <- gen_gridcube(n = c(10, 10, 10, 10), p = 4)
gen_gridcube <- function(n = 625, p = 4) {

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  # if (length(n) != p) {
  #   stop(cli::cli_alert_danger("n should contain exactly p values."))
  # }

  if (length(n) <= p) {
    n_vec <- rep(n^(1/p), p)
  }

  dims <- as.list(n_vec)
  df <- tidyr::expand_grid(!!!purrr::map(dims, seq_len))

  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate Cube with uniform points
#'
#' This function generates a grid dataset with specified uniform points along each axes..
#'
#' @param n A numeric vector (default: c(10, 10)) representing the number of grid points along each axes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the cube with uniform points.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' unif_cube <- gen_unifcube(n = 625, p = 4)
gen_unifcube <- function(n = 625, p = 4) {

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df <- matrix(runif(n * p, min = -0.5, max = 0.5),
                   nrow = n, ncol = p)
  df <- tibble::as_tibble(df)

  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)


}
