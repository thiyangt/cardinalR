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
#' one_grid <- gen_gridcube(n = 500, p = 4)
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
#' unif_cube <- gen_unifcube(n = 500, p = 4)
gen_unifcube <- function(n = 500, p = 4) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- matrix(runif(n * p, min = -0.5, max = 0.5),
                   nrow = n, ncol = p)
  # Create the tibble
  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)


}
