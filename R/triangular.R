#' Generate 3D Triangular Prism
#'
#' This function generates 3D triangular prism  datasets.
#'
#' @param n A numeric value (default: 300) representing the sample size.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#' @return A data containing a triangular prism in 3D.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' triangular_3d_data <- gen_tri_prism(n = 300, p = 3)
gen_tri_prism <- function(n = 300, p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  trace_point <- stats::runif(3)
  corner_points <- matrix(c(c(0, 1, 0.5, 0.5), c(0, 0, 1, 0.5), c(0, 0, 0, 1)),
    ncol = 3
  )

  df <- tibble::tibble(
    x1 = rep(0, n),
    x2 = rep(0, n),
    x3 = rep(0, n)
  )

  for (i in 1:n) {
    trace_point <- (corner_points[sample(4, 1), ] + trace_point) / 2
    df[i, ] <- as.list(trace_point)
  }

  if (p > 3) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 3,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 4:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}


#' Generate Triangular Plane with Background Noise
#'
#' This function generates a triangular plane dataset with background noise dimensions.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the triangular plane dataset with background noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' triangular_plane_data <- gen_tri_plane_with_bkg(n = 300, p = 3)
gen_tri_plane_with_bkg <- function(n = 300, p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  trace_point <- stats::runif(2)
  corner_points <- matrix(c(c(0, 1, 0.5), c(0, 0, 1)), ncol = 2)

  df1 <- tibble::tibble(
    x1 = rep(0, n),
    x2 = rep(0, n)
  )

  for (i in 1:(n * 0.7)) {
    trace_point <- (corner_points[sample(3, 1), ] + trace_point) / 2
    df1[i, ] <- as.list(trace_point)
  }

  if (p > 2) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df1), num_noise = p - 2,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 3:p)
    df1 <- dplyr::bind_cols(df1, noise_mat)

  }

  df2 <- gen_bkg_noise(
    n = n * 0.3, num_dims = NCOL(df1), mean = 0.025,
    sd = 0.5
  )

  df <- dplyr::bind_rows(df1, df2, -df1)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
