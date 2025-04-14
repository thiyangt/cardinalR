#' Generate points on a plane in high-dimensions
#'
#' This function generates points on a plane in high-dimensions.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the generated points on the plane.
#'
#' @examples
#' set.seed(20240412)
#' plane_points <- gen_hyperplane(n = 500, p = 4)
#'
#' @export
gen_hyperplane <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  # Generate n-1 random dimensions
  x <- matrix(stats::runif(n * (p - 1), -1, 1), ncol = (p - 1))

  # Compute the last coordinate to satisfy the plane equation
  coeff <- rep(3, p)
  intercept <- 0
  x_last <- (intercept - x %*% coeff[1:(p- 1)]) / coeff[p]

  # Combine all coordinates
  plane_points <- tibble::as_tibble(cbind(x, x_last)) |>
    rlang::set_names(paste0("x", 1:p))

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(plane_points)

}

#' Generate Hyper Plane with Hole
#'
#' This function generates a dataset representing a hyper plane with a hole.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the hyperplane data with a hole.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' plane_hole_data <- gen_hyperplane_with_hole(n = 1000, p = 4)
gen_hyperplaneHole <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  plane_points <- gen_hyperplane(n = n, p = p)

  # Compute Euclidean distance from the center
  distances <- sqrt(rowSums(plane_points^2))

  # Remove points inside the hole
  hole_radius <- 1
  plane_points <- plane_points |>
    dplyr::filter(distances > hole_radius)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(plane_points)

}


#' Generate Long Cluster Data
#'
#' This function generates a dataset consisting of any number of long clusters.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 3) representing the number of clusters.
#' @return A data containing the long cluster data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' long_cluster <- gen_clusts_long(n = c(200, 500, 300), p = 4, k = 3)
gen_clusts_long <- function(n = c(200, 500, 300), p = 4, k = 3) {

  if (k < 2) {
    cli::cli_abort("k should be greater than 2.")
  }

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != k) {
    cli::cli_abort("n should contain exactly {.val {k}} values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df <- tibble::tibble()

  for (j in 1:k) {

    data_list <- lapply(1:p, function(i) rep(0, n[j]))
    names(data_list) <- paste0("x", 1:p)

    df1 <- tibble::tibble(!!!data_list)

    scale_fac_vec <- stats::runif(p, -10, 10)
    shift_fac_vec <- stats::runif(p, -300, 300)

    for (i in 1:p) {

      df1[, i] <- scale_fac_vec[i] * (0:(n[j] - 1) + 0.03 * n[j] * stats::rnorm(n[j]) + shift_fac_vec[i])

    }

    df1 <- df1 |>
      dplyr::mutate(cluster = paste0("cluster", j))

    df <- dplyr::bind_rows(df, df1)

  }

  ## To swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
