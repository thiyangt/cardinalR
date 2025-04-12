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
#' tree_data <- gen_threebranches(n = c(200, 500, 300), p = 4)
gen_threebranches <- function(n = c(200, 500, 300), p = 4) {

  if (p < 3) {
     cli::cli_abort("p should be greater than 3.")

  }

  if (length(n) != 3) {
     cli::cli_abort("n should contain exactly 3 values.")
  }

  if (any(n < 0)) {
     cli::cli_abort("Values in n should be positive.")
  }

  x1 <- stats::runif(n[1], -2, 2)
  x2 <- -(x1^3 + stats::runif(n[1], 0, 6)) + stats::runif(n[1], 0, 0.2)
  x3 <- stats::rnorm(n[1], 10, 0.1)
  x4 <- stats::rnorm(n[1], 10, 0.1)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster1")

  x1 <- stats::runif(n[2], 0, 2)
  x2 <- (x1^3 + stats::runif(n[2], 0, 6)) + stats::runif(n[2], 0, 0.2)
  x3 <- stats::rnorm(n[2], 10, 0.1)
  x4 <- stats::rnorm(n[2], 10, 0.1)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster2")

  x1 <- stats::runif(n[3], -2, 0)
  x2 <- -(x1^3 + stats::runif(n[3], 0, 6)) + stats::runif(n[3], 0, 0.2) + 10
  x3 <- stats::rnorm(n[3], 10, 0.1)
  x4 <- stats::rnorm(n[3], 10, 0.1)

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster3")

  df <- dplyr::bind_rows(df1, df2, df3)

  if (p > 5) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4))
    colnames(noise_df) <- paste0("x", 5:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)

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
#' tree_data <- gen_fivebranches(n = c(200, 100, 300, 400, 300), p = 4)
gen_fivebranches <- function(n = c(200, 100, 300, 400, 300), p = 4) {

  if (p < 4) {
     cli::cli_abort("p should be 4 or greater.")

  }

  if (length(n) != 5) {
     cli::cli_abort("n should contain exactly 5 values.")
  }

  if (any(n < 0)) {
     cli::cli_abort("Values in n should be positive.")
  }

  x1 <- stats::runif(n[1], -3, 3)
  x2 <- abs(0.5 * x1)
  x3 <- stats::rnorm(n[1], 10, 0.03)
  x4 <- stats::rnorm(n[1], 10, 0.03)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster1")

  x1 <- stats::runif(n[2], -0.5, 0.5)
  x2 <- abs(10 * x1)
  x3 <- stats::rnorm(n[2], 10, 0.03)
  x4 <- stats::rnorm(n[2], 10, 0.03)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster2")

  x1 <- stats::runif(n[3], -6, 3)
  x2 <- (-1) * abs(0.5 * x1 + 5)
  x3 <- stats::rnorm(n[3], 10, 0.03)
  x4 <- stats::rnorm(n[3], 10, 0.03)

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster3")

  x1 <- stats::runif(n[4], -0.5, 0.5)
  x2 <- (-1) * abs(10 * x1) - 5
  x3 <- stats::rnorm(n[4], 10, 0.03)
  x4 <- stats::rnorm(n[4], 10, 0.03)

  df4 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster4")

  x1 <- stats::runif(n[5], -5, 5)
  x2 <- x1
  x3 <- stats::rnorm(n[5], 10, 0.03)
  x4 <- stats::rnorm(n[5], 10, 0.03)

  df5 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster5")

  df <- dplyr::bind_rows(df1, df2, df3, df4, df5)

  if (p > 5) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4))
    colnames(noise_df) <- paste0("x", 5:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)

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
#' seven_branching_data <- gen_sevenbranches(
#' n = c(200, 100, 250, 300, 150, 400, 50), p = 4)
gen_sevenbranches <- function(n = c(200, 100, 250, 300, 150, 400, 50), p = 4) {

  if (p < 4) {
     cli::cli_abort("p should be 4 or greater.")

  }

  if (length(n) != 7) {
     cli::cli_abort("n should contain exactly 7 values.")
  }

  if (any(n < 0)) {
     cli::cli_abort("Values in n should be positive.")
  }

  x1 <- stats::runif(n[1], -2, 2)
  x2 <- -(x1^3 + stats::runif(n[1], 0, 1)) + stats::runif(n[1], 0, 0.2)
  x3 <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)
  x4 <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster1")

  x1 <- stats::runif(n[2], -2, 1.5)
  x2 <- (x1^3 + stats::runif(n[2], 0, 1)) + stats::runif(n[2], 0, 0.2)
  x3 <- rep(0, n[2]) + stats::rnorm(n[2], 10, 0.03)
  x4 <- rep(0, n[2]) - stats::rnorm(n[2], 10, 0.03)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster2")

  x1 <- stats::runif(n[3], -2, 1.5)
  x2 <- (1 + (x1 - 3)^2 + stats::runif(n[3], 0, 1)) + stats::runif(n[3], 0, 0.1)
  x3 <- rep(0, n[3]) + stats::rnorm(n[3], 10, 0.03)
  x4 <- rep(0, n[3]) - stats::rnorm(n[3], 10, 0.03)

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster3")

  x1 <- stats::runif(n[4], -0.5, 3)
  x2 <- (1 + -(x1 - 3)^2 + stats::runif(n[4], 0, 1)) + stats::runif(n[4], 0, 0.1)
  x3 <- rep(0, n[4]) + stats::rnorm(n[4], 10, 0.03)
  x4 <- rep(0, n[4]) - stats::rnorm(n[4], 10, 0.03)

  df4 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster4")

  x1 <- stats::runif(n[5], -1, 1)
  x2 <- (20 + x1^3 + stats::runif(n[5], 0, 0.1)) + stats::runif(n[5], 0, 0.01)
  x3 <- rep(0, n[5]) + stats::rnorm(n[5], 10, 0.03)
  x4 <- rep(0, n[5]) - stats::rnorm(n[5], 10, 0.03)

  df5 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster5")

  x1 <- stats::runif(n[6], -2, 2)
  x2 <- (x1^2 + stats::runif(n[6], 0, 0.1)) + stats::runif(n[6], 0, 0.01) + 10
  x3 <- rep(0, n[6]) + stats::rnorm(n[6], 10, 0.03)
  x4 <- rep(0, n[6]) - stats::rnorm(n[6], 10, 0.03)

  df6 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster6")

  x1 <- stats::runif(n[7], -2, 2)
  x2 <- (x1^2 + stats::runif(n[7], 0, 0.2)) + stats::runif(n[7], 0, 0.01) + 15
  x3 <- rep(0, n[7]) + stats::rnorm(n[7], 10, 0.03)
  x4 <- rep(0, n[7]) - stats::rnorm(n[7], 10, 0.03)

  df7 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster7")

  df <- dplyr::bind_rows(df1, df2, df3, df4, df5, df6, df7)

  if (p > 5) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4))
    colnames(noise_df) <- paste0("x", 5:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)


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
#' four_branching_data <- gen_fourbranches(n = c(200, 300, 150, 250), p = 4)
gen_fourbranches <- function(n = c(200, 300, 150, 250), p = 4) {

  if (p < 4) {
     cli::cli_abort("p should be 4 or greater.")

  }

  if (length(n) != 4) {
     cli::cli_abort("n should contain exactly 4 values.")
  }

  if (any(n < 0)) {
     cli::cli_abort("Values in n should be positive.")
  }

  x1 <- stats::runif(n[1], -5, 1)
  x2 <- (exp(x1) + stats::runif(n[1], 0, 0.1)) + stats::runif(n[1], 0, 0.2)
  x3 <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)
  x4 <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster1")

  x1 <- stats::runif(n[2], -1, 5)
  x2 <- (exp(-x1) + stats::runif(n[2], 0, 0.1)) + stats::runif(n[2], 0, 0.2)
  x3 <- rep(0, n[2]) + stats::rnorm(n[2], 10, 0.03)
  x4 <- rep(0, n[2]) - stats::rnorm(n[2], 10, 0.03)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster2")

  x1 <- stats::runif(n[3], 0, 5)
  x2 <- (log(x1) + stats::runif(n[3], 0, 0.1)) + stats::runif(n[3], 0, 0.2)
  x3 <- rep(0, n[3]) + stats::rnorm(n[3], 10, 0.03)
  x4 <- rep(0, n[3]) - stats::rnorm(n[3], 10, 0.03)

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster3")

  x1 <- stats::runif(n[4], -5, 0)
  x2 <- (log(-x1) + stats::runif(n[4], 0, 0.1)) + stats::runif(n[4], 0, 0.2)
  x3 <- rep(0, n[4]) + stats::rnorm(n[4], 10, 0.03)
  x4 <- rep(0, n[4]) - stats::rnorm(n[4], 10, 0.03)

  df4 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster4")

  x1 <- stats::runif(sum(n) * 0.1, -5, 0)
  x2 <- stats::runif(sum(n) * 0.1, 0, 0.8) + stats::runif(sum(n) * 0.1, 0, 0.8)
  x3 <- rep(0, sum(n) * 0.1) + stats::rnorm(sum(n) * 0.1, 10, 0.03)
  x4 <- rep(0, sum(n) * 0.1) - stats::rnorm(sum(n) * 0.1, 10, 0.03)

  df5 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster5")

  df <- dplyr::bind_rows(df1, df2, df3, df4, df5)

  if (p > 5) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4))
    colnames(noise_df) <- paste0("x", 5:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)

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
#' branching_data <- gen_eightbranches(n = c(200, 300, 150, 250, 100, 100, 100, 100), p = 4)
gen_eightbranches <- function(n = c(200, 300, 150, 250, 100, 100, 100, 100), p = 4) {

  if (p < 4) {
     cli::cli_abort("p should be 4 or greater.")

  }

  if (length(n) != 8) {
     cli::cli_abort("n should contain exactly 5 values.")
  }

  if (any(n < 0)) {
     cli::cli_abort("Values in n should be positive.")
  }

  x1 <- stats::runif(n[1], -1, 2)
  x2 <- (exp(x1) + stats::runif(n[1], 0, 0.1)) + stats::runif(n[1])
  x3 <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)
  x4 <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster1")

  x1 <- stats::runif(n[2], -1, 1)
  x2 <- (exp(2 * x1) + stats::runif(n[2], 0, 0.1)) + stats::runif(n[2], 0, 0.2)
  x3 <- rep(0, n[2]) + stats::rnorm(n[2], 10, 0.03)
  x4 <- rep(0, n[2]) - stats::rnorm(n[2], 10, 0.03)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster2")

  x1 <- stats::runif(n[3], -1, 0.6)
  x2 <- (exp(3 * x1) + stats::runif(n[3], 0, 0.1)) + stats::runif(n[3], 0, 0.2)
  x3 <- rep(0, n[3]) + stats::rnorm(n[3], 10, 0.03)
  x4 <- rep(0, n[3]) - stats::rnorm(n[3], 10, 0.03)

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster3")

  x1 <- stats::runif(n[4], -1, 3)
  x2 <- (exp(0.5 * x1) + stats::runif(n[4], 0, 0.1)) + stats::runif(n[4], 0, 0.2)
  x3 <- rep(0, n[4]) + stats::rnorm(n[4], 10, 0.03)
  x4 <- rep(0, n[4]) - stats::rnorm(n[4], 10, 0.03)

  df4 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster4")

  x1 <- stats::runif(n[5], -2, 1)
  x2 <- (exp(-x1) + stats::runif(n[5], 0, 0.1)) + stats::runif(n[5], 0, 0.2)
  x3 <- rep(0, n[5]) + stats::rnorm(n[5], 10, 0.03)
  x4 <- rep(0, n[5]) - stats::rnorm(n[5], 10, 0.03)

  df5 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster5")

  x1 <- stats::runif(n[6], -1, 1)
  x2 <- (exp(2 * -x1) + stats::runif(n[6], 0, 0.1)) + stats::runif(n[6], 0, 0.2)
  x3 <- rep(0, n[6]) + stats::rnorm(n[6], 10, 0.03)
  x4 <- rep(0, n[6]) - stats::rnorm(n[6], 10, 0.03)

  df6 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster6")

  x1 <- stats::runif(n[7], -0.6, 1)
  x2 <- (exp(3 * -x1) + stats::runif(n[7], 0, 0.1)) + stats::runif(n[7], 0, 0.2)
  x3 <- rep(0, n[7]) + stats::rnorm(n[7], 10, 0.03)
  x4 <- rep(0, n[7]) - stats::rnorm(n[7], 10, 0.03)

  df7 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster7")

  x1 <- stats::runif(n[8], -3, 1)
  x2 <- (exp(0.5 * -x1) + stats::runif(n[8], 0, 0.1)) + stats::runif(n[8], 0, 0.2)
  x3 <- rep(0, n[8]) + stats::rnorm(n[8], 10, 0.03)
  x4 <- rep(0, n[8]) - stats::rnorm(n[8], 10, 0.03)

  df8 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4,
                        cluster = "cluster8")

  df <- dplyr::bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)

  if (p > 5) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4))
    colnames(noise_df) <- paste0("x", 5:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}
