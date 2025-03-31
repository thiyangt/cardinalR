#' Generate One Grid Data
#'
#' This function generates a grid dataset with specified grid points along each axes..
#'
#' @param n A numeric vector (default: c(10, 10)) representing the number of grid points along each axes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the one grid data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' one_grid <- gen_one_grid(n = c(10, 10, 10, 10), p = 4)
gen_one_grid <- function(n = c(10, 10, 10, 10), p = 4) {

  if (length(n) != p) {
    stop(cli::cli_alert_danger("n should contain exactly p values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  dims <- as.list(n)
  df <- tidyr::expand_grid(!!!purrr::map(dims, seq_len))

  names(df) <- paste0("x", 1:NCOL(df))

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)


}

#' Generate One Grid with Background Noise
#'
#' This function generates a grid data and background noise.
#'
#' @param n A numeric vector (default: c(10, 10)) representing the number of grid points along each axes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing one grid data with background noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' one_grid_bkg <- gen_one_grid_with_bkg(n = c(10, 10, 10, 10), p = 4)
gen_one_grid_with_bkg <- function(n = c(10, 10, 10, 10), p = 4) {

  if (length(n) != p) {
    stop(cli::cli_alert_danger("n should contain exactly p values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df1 <- gen_one_grid(n = n, p = p)

  df2 <- gen_bkg_noise(n = NROW(df1) * 0.3, num_dims = NCOL(df1), mean = 2, sd = 3)
  df <- dplyr::bind_rows(df1, df2)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate One Grid with Different Offset
#'
#' This function generates two grids with an offset.
#'
#' @param n A numeric vector (default: c(10, 10)) representing the number of grid points along each axes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing two grids data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' two_grid_comb <- gen_two_grids_comb(n = c(10, 10, 10, 10), p = 4)
gen_two_grids_comb <- function(n = c(10, 10, 10, 10), p = 4) {

  if (length(n) != p) {
    stop(cli::cli_alert_danger("n should contain exactly p values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df1 <- gen_one_grid(n = n, p = p)
  df2 <- df1 + 3
  df <- dplyr::bind_rows(df1, df2)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}


#' Generate Two Grids with Background Noise
#'
#' This function generates two grids with background noise.
#'
#' @param n A numeric vector (default: c(10, 10)) representing the number of grid points along each axes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing two grids data with background noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' two_grid_comb_bkg <- gen_two_grids_comb_bkg(n = c(10, 10, 10, 10), p = 4)
gen_two_grids_comb_bkg <- function(n = c(10, 10, 10, 10), p = 4) {

  if (length(n) != p) {
    stop(cli::cli_alert_danger("n should contain exactly p values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df1 <- gen_one_grid(n = n, p = p)
  df3 <- df1 + 5
  df1 <- dplyr::bind_rows(df1, df3)

  df2 <- gen_bkg_noise(n = NROW(df1) * 0.1, num_dims = NCOL(df1), mean = 3, sd = 5)
  df <- dplyr::bind_rows(df1, df2)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

