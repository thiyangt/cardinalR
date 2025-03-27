#' Generate Three Circular Clusters with Noise
#'
#' This function generates three circular clusters in 4D space with added noise dimensions.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the three circular clusters with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' circular_clusters_data <- gen_three_circulars(n = c(200, 500, 300), p = 4)
gen_three_circulars <- function(n = c(200, 500, 300), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))

  }

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  theta1 <- stats::runif(n[1], 0.0, 2 * pi)
  x1 <- cos(theta1) + stats::rnorm(n[1], 10, 0.03)
  x2 <- sin(theta1) + stats::rnorm(n[1], 10, 0.03)
  x3 <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)
  x4 <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  theta2 <- stats::runif(n[2], 0.0, 2 * pi)
  x1 <- 0.5 * cos(theta2) + stats::rnorm(n[2], 10, 0.03)
  x2 <- 0.5 * sin(theta2) + stats::rnorm(n[2], 10, 0.03)
  x3 <- rep(0, n[2]) + stats::rnorm(n[2], 10, 0.03)
  x4 <- rep(0, n[2]) - stats::rnorm(n[2], 10, 0.03)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::rnorm(n[3], 10, 0.03)
  x2 <- stats::rnorm(n[3], 10, 0.03)
  x3 <- rep(0, n[3]) + stats::rnorm(n[3], 10, 0.03)
  x4 <- rep(0, n[3]) - stats::rnorm(n[3], 10, 0.03)

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

#' Generate Cell Cycle Data with Noise
#'
#' This function generates a cell cycle dataset with added noise dimensions.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the cell cycle data with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' cell_cycle_data <- cell_cycle(
#'   n = 300, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
cell_cycle <- function(n, num_noise, min_n, max_n) {
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

  # To check that the assigned n is divided by three
  if ((n %% 3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(n / 3)
  } else {
    cluster_size <- n / 3
  }


  r1 <- 2
  r2 <- 1

  theta <- stats::runif(cluster_size, 0, 2 * pi)
  x <- rep(0, cluster_size)
  y <- r1 * cos(theta)
  z <- r2 * sin(theta)

  df1 <- matrix(c(x, y, z), ncol = 3)

  x <- r2 * cos(theta)
  y <- rep(0, cluster_size)
  z <- r1 * sin(theta)

  df2 <- matrix(c(x, y, z), ncol = 3)

  x <- r1 * cos(theta)
  y <- r2 * sin(theta)
  z <- rep(0, cluster_size)

  df3 <- matrix(c(x, y, z), ncol = 3)

  df <- rbind(df1, df2, df3)

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

    df
  } else {
    df
  }
}

#' Generate Curvy Cell Cycle Data with Noise
#'
#' This function generates a curvy cell cycle dataset with added noise dimensions.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the curvy cell cycle data with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' curvy_cell_cycle_data <- curvy_cycle(
#'   n = 300, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
curvy_cycle <- function(n, num_noise, min_n, max_n) {
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

  # To check that the assigned n is divided by three
  if ((n %% 3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(n / 3)
  } else {
    cluster_size <- n / 3
  }


  r <- sqrt(3) / 3

  theta <- stats::runif(cluster_size, 0, 2 * pi)
  x <- cos(theta)
  y <- r + sin(theta)
  z <- cos(3 * theta) / 3

  df1 <- matrix(c(x, y, z), ncol = 3)

  x <- cos(theta) + 0.5
  y <- sin(theta) - r / 2
  z <- cos(3 * theta) / 3

  df2 <- matrix(c(x, y, z), ncol = 3)

  x <- cos(theta) - 0.5
  y <- sin(theta) - r / 2
  z <- cos(3 * theta) / 3

  df3 <- matrix(c(x, y, z), ncol = 3)

  df <- rbind(df1, df2, df3)

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

    df
  } else {
    df
  }
}

#' Generate Linked Data
#'
#' This function generates linked data points.
#'
#' @param n The total number of data points to be generated. Should be a product of two.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated linked data points.
#' @export
#'
#' @examples
#'
#' # Generate linked data with noise with custom parameters
#' set.seed(20240412)
#' data <- two_circulars(n = 200, num_noise = 2, min_n = -0.05, max_n = 0.05)
two_circulars <- function(n, num_noise, min_n, max_n) {
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

  # To check that the assigned n is divided by two
  if ((n %% 2) != 0) {
    warning("The sample size should be a product of two.")
    cluster_size <- floor(n / 2)
  } else {
    cluster_size <- n / 2
  }

  theta <- (0:(cluster_size - 1)) * (2 * pi / cluster_size)
  cs <- cos(.4)
  sn <- sin(.4)

  df1 <- matrix(c(
    cos(theta),
    cs * sin(theta),
    -sn * sin(theta)
  ), ncol = 3)

  df2 <- matrix(c(
    1 + cos(theta),
    sn * sin(theta),
    cs * sin(theta)
  ), ncol = 3)

  df <- rbind(df1, df2)

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

    df
  } else {
    df
  }
}
