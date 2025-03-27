#' Generate Three Circular Clusters
#'
#' This function generates a dataset representing a structure with three circulars.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the three circular clusters.
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
  theta2 <- stats::runif(n[1], 0.0, 2 * pi)
  x2 <- sin(theta2) + stats::rnorm(n[1], 10, 0.03)
  x3 <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)
  x4 <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  theta3 <- stats::runif(n[2], 0.0, 2 * pi)
  x1 <- 0.5 * cos(theta3) + stats::rnorm(n[2], 10, 0.03)
  theta4 <- stats::runif(n[2], 0.0, 2 * pi)
  x2 <- 0.5 * sin(theta4) + stats::rnorm(n[2], 10, 0.03)
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

#' Generate Three Cell Cycle Data
#'
#' This function generates a dataset representing a structure with three cell cycles.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#' @return A data containing the three cell cycle data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' cell_cycle_data <- gen_three_cell_cycle(n = c(200, 500, 300), p = 3)
gen_three_cell_cycle <- function(n = c(200, 500, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }


  r1 <- 2
  r2 <- 1

  theta1 <- stats::runif(n[1], 0, 2 * pi)
  x1 <- rep(0, n[1])
  x2 <- r1 * cos(theta1)
  theta2 <- stats::runif(n[1], 0, 2 * pi)
  x3 <- r2 * sin(theta2)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  theta3 <- stats::runif(n[2], 0, 2 * pi)
  x1 <- r2 * cos(theta3)
  x2 <- rep(0, n[2])
  theta4 <- stats::runif(n[2], 0, 2 * pi)
  x3 <- r1 * sin(theta4)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  theta5 <- stats::runif(n[3], 0, 2 * pi)
  x1 <- r1 * cos(theta5)
  theta6 <- stats::runif(n[3], 0, 2 * pi)
  x2 <- r2 * sin(theta6)
  x3 <- rep(0, n[3])

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  df <- dplyr::bind_rows(df1, df2, df3)

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

#' Generate Three Curvy Cell Cycle Data
#'
#' This function generates a dataset representing a structure with three curvy cell cycles,
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#' @return A data containing the three curvy cell cycles.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' curvy_cell_cycle_data <- gen_three_curvy_cycle(n = c(200, 500, 300), p = 3)
gen_three_curvy_cycle <- function(n = c(200, 500, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }


  r <- sqrt(3) / 3

  theta1 <- stats::runif(n[1], 0, 2 * pi)
  x1 <- cos(theta1)
  theta2 <- stats::runif(n[1], 0, 2 * pi)
  x2 <- r + sin(theta2)
  theta3 <- stats::runif(n[1], 0, 2 * pi)
  x3 <- cos(3 * theta3) / 3

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  theta4 <- stats::runif(n[2], 0, 2 * pi)
  x1 <- cos(theta4) + 0.5
  theta5 <- stats::runif(n[2], 0, 2 * pi)
  x2 <- sin(theta5) - r / 2
  theta6 <- stats::runif(n[2], 0, 2 * pi)
  x3 <- cos(3 * theta6) / 3

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  theta7 <- stats::runif(n[3], 0, 2 * pi)
  x1 <- cos(theta7) - 0.5
  theta8 <- stats::runif(n[3], 0, 2 * pi)
  x2 <- sin(theta8) - r / 2
  theta9 <- stats::runif(n[3], 0, 2 * pi)
  x3 <- cos(3 * theta9) / 3

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  df <- dplyr::bind_rows(df1, df2, df3)

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

#' Generate Two Link Circular Clusters
#'
#' This function generates a dataset representing a structure with two link circular clusters.
#'
#' @param n A numeric vector (default: c(200, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#'
#' @return A matrix containing the generated two linked circular clusters.
#' @export
#'
#' @examples
#'
#' # Generate linked data with noise with custom parameters
#' set.seed(20240412)
#' data <- gen_two_circulars(n = c(200, 300), p = 3)
gen_two_circulars <- function(n = c(200, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  theta1 <- (0:(n[1] - 1)) * (2 * pi / n[1])
  theta2 <- (0:(n[1] - 1)) * (2 * pi / n[1])
  theta3 <- (0:(n[1] - 1)) * (2 * pi / n[1])
  cs <- cos(.4)
  sn <- sin(.4)

  x1 <- cos(theta1)
  x2 <- cs * sin(theta2)
  x3 <- -sn * sin(theta3)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  theta4 <- (0:(n[2] - 1)) * (2 * pi / n[2])
  theta5 <- (0:(n[2] - 1)) * (2 * pi / n[2])
  theta6 <- (0:(n[2] - 1)) * (2 * pi / n[2])
  x1 <- 1 + cos(theta4)
  x2 <- sn * sin(theta5)
  x3 <- cs * sin(theta6)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  df <- dplyr::bind_rows(df1, df2)

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
