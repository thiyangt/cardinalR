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
gen_tri_prism <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  trace_point <- stats::runif(p)
  corner_points <- geozoo::simplex(p=p)$points

  data_list <- lapply(1:p, function(i) rep(0, n))
  names(data_list) <- paste0("x", 1:p)

  df <- tibble::tibble(!!!data_list)

  for (i in 1:n) {
    trace_point <- (corner_points[sample((p + 1), 1), ] + trace_point) / 2
    df[i, ] <- as.list(trace_point)
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
  corner_points <- geozoo::simplex(p=2)$points

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
