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
gen_gridcube <- function(n = c(10, 10, 10, 10), p = 4) {

  if (length(n) != p) {
    stop(cli::cli_alert_danger("n should contain exactly p values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  dims <- as.list(n)
  df <- tidyr::expand_grid(!!!purrr::map(dims, seq_len))

  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}


gen_unifcube <- function(n = c(10, 10, 10, 10), p = 4) {

  if (length(n) != p) {
    stop(cli::cli_alert_danger("n should contain exactly p values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df <- tidyr::expand_grid(!!!purrr::map(p, seq_len))

  names(df) <- paste0("x", 1:NCOL(df))

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)


}
