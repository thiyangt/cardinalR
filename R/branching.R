#' Generate data with three branches
#'
#' This function generates a dataset representing a structure with three branches.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing three branches.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' tree_data <- gen_three_branch_data(n = c(200, 500, 300), p = 4)
gen_three_branch_data <- function(n = c(200, 500, 300), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))

  }

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- stats::runif(n[1], -2, 2)
  x2 <- -(x1^3 + stats::runif(n[1], 0, 6)) + stats::runif(n[1], 0, 0.2)
  x3 <- stats::rnorm(n[1], 10, 0.1)
  x4 <- stats::rnorm(n[1], 10, 0.1)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[2], 0, 2)
  x2 <- (x1^3 + stats::runif(n[2], 0, 6)) + stats::runif(n[2], 0, 0.2)
  x3 <- stats::rnorm(n[2], 10, 0.1)
  x4 <- stats::rnorm(n[2], 10, 0.1)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[3], -2, 0)
  x2 <- -(x1^3 + stats::runif(n[3], 0, 6)) + stats::runif(n[3], 0, 0.2) + 10
  x3 <- stats::rnorm(n[3], 10, 0.1)
  x4 <- stats::rnorm(n[3], 10, 0.1)

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- bind_rows(df1, df2, df3)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate data with five branches
#'
#' This function generates a dataset representing a structure with five branches.
#'
#' @param n A numeric vector (default: c(200, 100, 300, 400, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing five branches.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' tree_data <- gen_five_branch_data(n = c(200, 100, 300, 400, 300), p = 4)
gen_five_branch_data <- function(n = c(200, 100, 300, 400, 300), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))

  }

  if (length(n) != 5) {
    stop(cli::cli_alert_danger("n should contain exactly 5 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- stats::runif(n[1], -3, 3)
  x2 <- abs(0.5 * x1)
  x3 <- stats::rnorm(n[1], 10, 0.03)
  x4 <- stats::rnorm(n[1], 10, 0.03)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[2], -0.5, 0.5)
  x2 <- abs(10 * x1)
  x3 <- stats::rnorm(n[2], 10, 0.03)
  x4 <- stats::rnorm(n[2], 10, 0.03)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[3], -6, 3)
  x2 <- (-1) * abs(0.5 * x1 + 5)
  x3 <- stats::rnorm(n[3], 10, 0.03)
  x4 <- stats::rnorm(n[3], 10, 0.03)

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[4], -0.5, 0.5)
  x2 <- (-1) * abs(10 * x1) - 5
  x3 <- stats::rnorm(n[4], 10, 0.03)
  x4 <- stats::rnorm(n[4], 10, 0.03)

  df4 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[5], -5, 5)
  x2 <- x1
  x3 <- stats::rnorm(n[5], 10, 0.03)
  x4 <- stats::rnorm(n[5], 10, 0.03)

  df5 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- bind_rows(df1, df2, df3, df4, df5)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate data with seven branches
#'
#' This function generates a dataset representing a structure with seven branches.
#'
#' @param n A numeric vector (default: c(200, 100, 250, 300, 150, 400, 50)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing seven branches.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' seven_branching_data <- gen_seven_branch_data(
#' n = c(200, 100, 250, 300, 150, 400, 50), p = 4)
gen_seven_branch_data <- function(n = c(200, 100, 250, 300, 150, 400, 50), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))

  }

  if (length(n) != 7) {
    stop(cli::cli_alert_danger("n should contain exactly 7 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- stats::runif(n[1], -2, 2)
  x2 <- -(x1^3 + stats::runif(n[1], 0, 1)) + stats::runif(n[1], 0, 0.2)
  x3 <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)
  x4 <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[2], -2, 1.5)
  x2 <- (x1^3 + stats::runif(n[2], 0, 1)) + stats::runif(n[2], 0, 0.2)
  x3 <- rep(0, n[2]) + stats::rnorm(n[2], 10, 0.03)
  x4 <- rep(0, n[2]) - stats::rnorm(n[2], 10, 0.03)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[3], -2, 1.5)
  x2 <- (1 + (x1 - 3)^2 + stats::runif(n[3], 0, 1)) + stats::runif(n[3], 0, 0.1)
  x3 <- rep(0, n[3]) + stats::rnorm(n[3], 10, 0.03)
  x4 <- rep(0, n[3]) - stats::rnorm(n[3], 10, 0.03)

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[4], -0.5, 3)
  x2 <- (1 + -(x1 - 3)^2 + stats::runif(n[4], 0, 1)) + stats::runif(n[4], 0, 0.1)
  x3 <- rep(0, n[4]) + stats::rnorm(n[4], 10, 0.03)
  x4 <- rep(0, n[4]) - stats::rnorm(n[4], 10, 0.03)

  df4 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[5], -1, 1)
  x2 <- (20 + x1^3 + stats::runif(n[5], 0, 0.1)) + stats::runif(n[5], 0, 0.01)
  x3 <- rep(0, n[5]) + stats::rnorm(n[5], 10, 0.03)
  x4 <- rep(0, n[5]) - stats::rnorm(n[5], 10, 0.03)

  df5 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[6], -2, 2)
  x2 <- (x1^2 + stats::runif(n[6], 0, 0.1)) + stats::runif(n[6], 0, 0.01) + 10
  x3 <- rep(0, n[6]) + stats::rnorm(n[6], 10, 0.03)
  x4 <- rep(0, n[6]) - stats::rnorm(n[6], 10, 0.03)

  df6 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[7], -2, 2)
  x2 <- (x1^2 + stats::runif(n[7], 0, 0.2)) + stats::runif(n[7], 0, 0.01) + 15
  x3 <- rep(0, n[7]) + stats::rnorm(n[7], 10, 0.03)
  x4 <- rep(0, n[7]) - stats::rnorm(n[7], 10, 0.03)

  df7 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- rbind(df1, df2, df3, df4, df5, df6, df7)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate data with four branches
#'
#' This function generates a dataset representing a structure with four branches.
#'
#' @param n A numeric vector (default: c(200, 300, 150, 250)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing four branches.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' four_branching_data <- gen_four_branch_data(n = c(200, 300, 150, 250), p = 4)
gen_four_branch_data <- function(n = c(200, 300, 150, 250), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))

  }

  if (length(n) != 4) {
    stop(cli::cli_alert_danger("n should contain exactly 4 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- stats::runif(n[1], -5, 1)
  x2 <- (exp(x1) + stats::runif(n[1], 0, 0.1)) + stats::runif(n[1], 0, 0.2)
  x3 <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)
  x4 <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[2], -1, 5)
  x2 <- (exp(-x1) + stats::runif(n[2], 0, 0.1)) + stats::runif(n[2], 0, 0.2)
  x3 <- rep(0, n[2]) + stats::rnorm(n[2], 10, 0.03)
  x4 <- rep(0, n[2]) - stats::rnorm(n[2], 10, 0.03)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[3], 0, 5)
  x2 <- (log(x1) + stats::runif(n[3], 0, 0.1)) + stats::runif(n[3], 0, 0.2)
  x3 <- rep(0, n[3]) + stats::rnorm(n[3], 10, 0.03)
  x4 <- rep(0, n[3]) - stats::rnorm(n[3], 10, 0.03)

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[4], -5, 0)
  x2 <- (log(-x1) + stats::runif(n[4], 0, 0.1)) + stats::runif(n[4], 0, 0.2)
  x3 <- rep(0, n[4]) + stats::rnorm(n[4], 10, 0.03)
  x4 <- rep(0, n[4]) - stats::rnorm(n[4], 10, 0.03)

  df4 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(sum(n) * 0.1, -5, 0)
  x2 <- stats::runif(sum(n) * 0.1, 0, 0.8) + stats::runif(sum(n) * 0.1, 0, 0.8)
  x3 <- rep(0, sum(n) * 0.1) + stats::rnorm(sum(n) * 0.1, 10, 0.03)
  x4 <- rep(0, sum(n) * 0.1) - stats::rnorm(sum(n) * 0.1, 10, 0.03)

  df5 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- bind_rows(df1, df2, df3, df4, df5)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate data with eight branches
#'
#' This function generates a dataset representing a structure with eight branches.
#'
#' @param n A numeric vector (default: c(200, 300, 150, 250, 100, 100, 100, 100)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing eight branches.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' branching_data <- gen_eight_branch_data(n = c(200, 300, 150, 250, 100, 100, 100, 100), p = 4)
gen_eight_branch_data <- function(n = c(200, 300, 150, 250, 100, 100, 100, 100), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))

  }

  if (length(n) != 8) {
    stop(cli::cli_alert_danger("n should contain exactly 5 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- stats::runif(n[1], -1, 2)
  x2 <- (exp(x1) + stats::runif(n[1], 0, 0.1)) + stats::runif(n[1])
  x3 <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)
  x4 <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[2], -1, 1)
  x2 <- (exp(2 * x1) + stats::runif(n[2], 0, 0.1)) + stats::runif(n[2], 0, 0.2)
  x3 <- rep(0, n[2]) + stats::rnorm(n[2], 10, 0.03)
  x4 <- rep(0, n[2]) - stats::rnorm(n[2], 10, 0.03)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[3], -1, 0.6)
  x2 <- (exp(3 * x1) + stats::runif(n[3], 0, 0.1)) + stats::runif(n[3], 0, 0.2)
  x3 <- rep(0, n[3]) + stats::rnorm(n[3], 10, 0.03)
  x4 <- rep(0, n[3]) - stats::rnorm(n[3], 10, 0.03)

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[4], -1, 3)
  x2 <- (exp(0.5 * x1) + stats::runif(n[4], 0, 0.1)) + stats::runif(n[4], 0, 0.2)
  x3 <- rep(0, n[4]) + stats::rnorm(n[4], 10, 0.03)
  x4 <- rep(0, n[4]) - stats::rnorm(n[4], 10, 0.03)

  df4 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[5], -2, 1)
  x2 <- (exp(-x1) + stats::runif(n[5], 0, 0.1)) + stats::runif(n[5], 0, 0.2)
  x3 <- rep(0, n[5]) + stats::rnorm(n[5], 10, 0.03)
  x4 <- rep(0, n[5]) - stats::rnorm(n[5], 10, 0.03)

  df5 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[6], -1, 1)
  x2 <- (exp(2 * -x1) + stats::runif(n[6], 0, 0.1)) + stats::runif(n[6], 0, 0.2)
  x3 <- rep(0, n[6]) + stats::rnorm(n[6], 10, 0.03)
  x4 <- rep(0, n[6]) - stats::rnorm(n[6], 10, 0.03)

  df6 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[7], -0.6, 1)
  x2 <- (exp(3 * -x1) + stats::runif(n[7], 0, 0.1)) + stats::runif(n[7], 0, 0.2)
  x3 <- rep(0, n[7]) + stats::rnorm(n[7], 10, 0.03)
  x4 <- rep(0, n[7]) - stats::rnorm(n[7], 10, 0.03)

  df7 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::runif(n[8], -3, 1)
  x2 <- (exp(0.5 * -x1) + stats::runif(n[8], 0, 0.1)) + stats::runif(n[8], 0, 0.2)
  x3 <- rep(0, n[8]) + stats::rnorm(n[8], 10, 0.03)
  x4 <- rep(0, n[8]) - stats::rnorm(n[8], 10, 0.03)

  df8 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- dplyr::bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)

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

#' Generate Curvy Branching Cluster Data
#'
#' This function generates two curvy clusters and one Gaussian cluster in the middle.
#'
#' @param n A numeric vector (default: c(200, 200, 100)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing two curvy clusters and one Gaussian cluster.
#' @export
#'
#' @examples
#'
#' # Generate curvy branching cluster data with custom parameters
#' set.seed(20240412)
#' data <- gen_curvy_branch_clust(n = c(200, 200, 100), p = 4)
gen_curvy_branch_clust <- function(n = c(200, 200, 100), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  theta <- stats::runif(n[1], 0.20, 0.90 * pi)


  x1 <- cos(theta) + stats::rnorm(n[1], 1, 0.06)
  x2 <- sin(theta) + stats::rnorm(n[1], 1, 0.06)
  x3 <- cos(theta) + stats::rnorm(n[1], 1, 0.06)
  x4 <- sin(theta) + stats::rnorm(n[1], 1, 0.06)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)


  theta1 <- stats::runif(n[2], 0.20, 0.90 * pi)


  x1 <- cos(-theta1) + stats::rnorm(n[2], 1, 0.06)
  x2 <- sin(-theta1) + stats::rnorm(n[2], 1, 0.06)
  x3 <- cos(-theta1) + stats::rnorm(n[2], 1, 0.06)
  x4 <- sin(-theta1) + stats::rnorm(n[2], 1, 0.06)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)


  x1 <- stats::rnorm(n[3], mean = 1, sd = 0.08)
  x2 <- stats::rnorm(n[3], mean = 1, sd = 0.08)
  x3 <- stats::rnorm(n[3], mean = 1, sd = 0.08)
  x4 <- stats::rnorm(n[3], mean = 1, sd = 0.08)


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

#' Generate Curvy Branching Cluster Data with Background Noise
#'
#' This function generates data with four clusters, two of which follow a
#' curvilinear pattern and the other two are distributed randomly.
#'
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate curvy branching cluster data with background noise with custom parameters
#' set.seed(20240412)
#' data <- curvy_branch_clust_bkg(
#'   n = 400, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
curvy_branch_clust_bkg <- function(n, num_noise, min_n, max_n) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))

  }

  if (length(n) != 4) {
    stop(cli::cli_alert_danger("n should contain exactly 4 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }


  theta <- stats::runif(cluster_size, 0.20, 0.90 * pi)

  df1 <- matrix(c(
    cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    sin(theta) + stats::rnorm(cluster_size, 1, 0.06),
    cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    sin(theta) + stats::rnorm(cluster_size, 1, 0.06)
  ), ncol = 4)

  theta1 <- stats::runif(cluster_size, 0.20, 0.90 * pi)

  df2 <- matrix(c(
    cos(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    sin(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    cos(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    sin(-theta1) + stats::rnorm(cluster_size, 1, 0.06)
  ), ncol = 4)

  df3 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 0.08),
    stats::rnorm(cluster_size, mean = 1, sd = 0.08),
    stats::rnorm(cluster_size, mean = 1, sd = 0.08),
    stats::rnorm(cluster_size, mean = 1, sd = 0.08)
  ), ncol = 4)

  df4 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 1),
    stats::rnorm(cluster_size, mean = 1, sd = 1),
    stats::rnorm(cluster_size, mean = 1, sd = 1),
    stats::rnorm(cluster_size, mean = 1, sd = 1)
  ), ncol = 4)

  df <- bind_rows(df1, df2, df3, df4)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate Curvy Branching Clusters with Noise
#'
#' This function generates data with curvy branching clusters along with added noise.
#'
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate curvy branching clusters with noise with custom parameters
#' set.seed(20240412)
#' data <- gen_two_curvy(n = c(300, 200), p = 4)
gen_two_curvy <- function(n = c(300, 200), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }


  theta1 <- stats::runif(n[1], 0.20, 0.90 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta1) + stats::rnorm(n[1], 1, 0.06),
    x2 = sin(theta1) + stats::rnorm(n[1], 1, 0.06),
    x3 = cos(theta1) + stats::rnorm(n[1], 1, 0.06),
    x4 = sin(theta1) + stats::rnorm(n[1], 1, 0.06)
  )

  theta2 <- stats::runif(n[2], 0.20, 0.90 * pi)

  df2 <- tibble::tibble(
    x1 = cos(-theta2) + stats::rnorm(n[2], 1, 0.06),
    x2 = sin(-theta2) + stats::rnorm(n[2], 1, 0.06),
    x3 = cos(-theta2) + stats::rnorm(n[2], 1, 0.06),
    x4 = sin(-theta2) + stats::rnorm(n[2], 1, 0.06)
  )

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
