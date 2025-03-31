#' Generate points on a plane in high-dimensions
#'
#' This function generates points on a plane in high-dimensions.
#'
#' @param n A numeric value representing the sample size.
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

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  # Generate n-1 random dimensions
  x <- matrix(runif(n * (p - 1), -1, 1), ncol = (p - 1))

  # Compute the last coordinate to satisfy the plane equation
  coeff <- rep(3, p)
  intercept <- 0
  x_last <- (intercept - x %*% coeff[1:(p- 1)]) / coeff[p]

  # Combine all coordinates
  plane_points <- tibble::as_tibble(cbind(x, x_last)) %>%
    set_names(paste0("x", 1:p))

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(plane_points)

}

#' Generate Long Cluster Data
#'
#' This function generates a dataset consisting of two long clusters.
#'
#' @param n A numeric vector (default: c(200, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the long cluster data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' long_cluster <- gen_two_long_clusts(n = c(200, 300), p = 4)
gen_two_long_clusts <- function(n = c(200, 300), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  x2 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  x3 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  x4 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x2 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5
  x3 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x4 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5
  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- dplyr::bind_rows(df1, df2)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Three Different Linear Data
#'
#' This function generates a dataset consisting of three different linear clusters.
#'
#' @param n A numeric vector (default: c(200, 300, 150)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the three different linear data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' three_diff_linear <- gen_three_angled_long_clusts(n = c(200, 300, 150), p = 4)
gen_three_angled_long_clusts <- function(n = c(200, 300, 150), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 150
  x2 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 20
  x3 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  x4 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 65

  df1 <- tibble::tibble(x1 = x1,
                         x2 = x2,
                         x3 = x3,
                         x4 = x4)

  x1 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x2 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5
  x3 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x4 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- -(0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]))
  x2 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3])
  x3 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3])
  x4 <- -(0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]))

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- dplyr::bind_rows(df1, df2, df3)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Four Different Long Clusters
#'
#' This function generates a dataset consisting of four different long clusters.
#'
#' @param n A numeric vector (default: c(200, 150, 300, 150)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the four different long clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' four_diff_long_clusters <- gen_four_long_clusts(n = c(200, 150, 300, 150), p = 4)
gen_four_long_clusts <- function(n = c(200, 150, 300, 150), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (length(n) != 4) {
    stop(cli::cli_alert_danger("n should contain exactly 4 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 150
  x2 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  x3 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 20
  x4 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 85

  df1 <- tibble::tibble(x1 = x1,
                         x2 = x2,
                         x3 = x3,
                         x4 = x4)

  x1 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x2 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5
  x3 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x4 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) - 70
  x2 <- -(0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]))
  x3 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3])
  x4 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) - 85

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[4] - 1) + 0.03 * n[4] * stats::rnorm(n[4]) - 70
  x2 <- -(0:(n[4] - 1) + 0.03 * n[4] * stats::rnorm(n[4])) + 150
  x3 <- 0:(n[4] - 1) + 0.03 * n[4] * stats::rnorm(n[4])
  x4 <- 0:(n[4] - 1) + 0.03 * n[4] * stats::rnorm(n[4]) + 85

  df4 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- dplyr::bind_rows(df1, df2, df3, df4)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate 2D Plane with Hole and Noise
#'
#' This function generates a dataset representing a 2D plane with a hole in the
#' middle, with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A list containing the 2D plane data with a hole and the sample size.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' plane_data <- plane_2d_hole(
#'   n = 100, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
plane_2d_hole <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by four
  if ((n %% 4) != 0) {
    stop("The sample size should be a product of four.")
  } else {
    cluster_size <- n / 4
  }

  u <- stats::runif(cluster_size, min = 10, max = 30)
  v <- stats::runif(cluster_size, min = 10, max = 20)
  x <- u + v - 10
  y <- v - u + 8
  df1 <- matrix(c(x, y), ncol = 2)

  anchor <- c(1, 1)
  indices <- rowSums((sweep(df1, 2, anchor, `-`))) > 20
  df1 <- df1[indices, ]
  rownames(df1) <- NULL

  df2 <- matrix(c(-df1[, 2] + 26, df1[, 1] - 15), ncol = 2)
  df3 <- matrix(c(df1[, 2] + 30, -df1[, 1] + 25), ncol = 2)

  df <- rbind(df1 - 10, df1 + 10, df2, df3)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)
  }

  return(list(df = df, n = NROW(df)))
}

#' Generate Three Linear Clusters
#'
#' This function generates data with three linear clusters.
#'
#' @param n A numeric vector (default: c(200, 300, 150)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the three long clusters.
#' @export
#'
#' @examples
#'
#' # Generate three linear clusters with noise with custom parameters
#' set.seed(20240412)
#' data <- gen_three_long_clusts(n = c(200, 300, 150), p = 4)
gen_three_long_clusts <- function(n = c(200, 300, 150), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) + 100
  x2 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 100
  x3 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 100
  x4 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) + 100

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x2 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5
  x3 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x4 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) - 10
  x2 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) + 10
  x3 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) - 10
  x4 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) + 10

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- dplyr::bind_rows(df1, df2, df3)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
