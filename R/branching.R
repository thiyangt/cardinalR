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

  df1 <- matrix(0, nrow = n[1], ncol = p)
  df1[, 1] <- stats::runif(n[1], -2, 2)
  df1[, 2] <- -(df1[, 1]^3 + stats::runif(n[1], 0, 6)) + stats::runif(n[1], 0, 0.2)
  df1[, 3] <- stats::rnorm(n[1], 10, 0.1)

  df2 <- matrix(0, nrow = n[2], ncol = p)
  df2[, 1] <- stats::runif(n[2], 0, 2)
  df2[, 2] <- (df2[, 1]^3 + stats::runif(n[2], 0, 6)) + stats::runif(n[2], 0, 0.2)
  df2[, 3] <- stats::rnorm(n[2], 10, 0.1)

  df3 <- matrix(0, nrow = n[3], ncol = p)
  df3[, 1] <- stats::runif(n[3], -2, 0)
  df3[, 2] <- -(df3[, 1]^3 + stats::runif(n[3], 0, 6)) + stats::runif(n[3], 0, 0.2) + 10
  df3[, 3] <- stats::rnorm(n[3], 10, 0.1)

  if(p == 3) {

    df1 <- tibble::as_tibble(df1, .name_repair = "minimal")
    df2 <- tibble::as_tibble(df2, .name_repair = "minimal")
    df3 <- tibble::as_tibble(df3, .name_repair = "minimal")

    names(df1) <- paste0("x", 1:3)
    names(df2) <- paste0("x", 1:3)
    names(df3) <- paste0("x", 1:3)

    df1 <- df1 |> dplyr::mutate(cluster = "cluster1")
    df2 <- df2 |> dplyr::mutate(cluster = "cluster2")
    df3 <- df3 |> dplyr::mutate(cluster = "cluster3")

    df <- dplyr::bind_rows(df1, df2, df3)

  } else { # p >=4

    df1[, 4] <- stats::rnorm(n[1], 10, 0.1)
    df2[, 4] <- stats::rnorm(n[2], 10, 0.1)
    df3[, 4] <- stats::rnorm(n[3], 10, 0.1)

    df1 <- tibble::as_tibble(df1, .name_repair = "minimal")
    df2 <- tibble::as_tibble(df2, .name_repair = "minimal")
    df3 <- tibble::as_tibble(df3, .name_repair = "minimal")
    names(df1) <- paste0("x", 1:4)
    names(df2) <- paste0("x", 1:4)
    names(df3) <- paste0("x", 1:4)

    df1 <- df1 |> dplyr::mutate(cluster = "cluster1")
    df2 <- df2 |> dplyr::mutate(cluster = "cluster2")
    df3 <- df3 |> dplyr::mutate(cluster = "cluster3")

    df <- dplyr::bind_rows(df1, df2, df3)
    if (p > 4) {

      noise_df <- gen_noisedims(n = NROW(df), p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4))
      names(noise_df) <- paste0("x", 5:p)

      df <- dplyr::bind_cols(df, noise_df) |>
        dplyr::select(dplyr::starts_with("x"), "cluster")

    }

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

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")

  }

  if (length(n) != 5) {
     cli::cli_abort("n should contain exactly 5 values.")
  }

  if (any(n < 0)) {
     cli::cli_abort("Values in n should be positive.")
  }

  df1 <- matrix(0, nrow = n[1], ncol = p)
  df1[, 1] <- stats::runif(n[1], -3, 3)
  df1[, 2] <- abs(0.5 * df1[, 1])
  df1[, 3] <- stats::rnorm(n[1], 10, 0.03)

  df2 <- matrix(0, nrow = n[2], ncol = p)
  df2[, 1] <- stats::runif(n[2], -0.5, 0.5)
  df2[, 2] <- abs(10 * df2[, 1])
  df2[, 3] <- stats::rnorm(n[2], 10, 0.03)

  df3 <- matrix(0, nrow = n[3], ncol = p)
  df3[, 1] <- stats::runif(n[3], -6, 3)
  df3[, 2] <- (-1) * abs(0.5 * df3[, 1] + 5)
  df3[, 3] <- stats::rnorm(n[3], 10, 0.03)

  df4 <- matrix(0, nrow = n[4], ncol = p)
  df4[, 1] <- stats::runif(n[4], -0.5, 0.5)
  df4[, 2] <- (-1) * abs(10 * df4[, 1]) - 5
  df4[, 3] <- stats::rnorm(n[4], 10, 0.03)

  df5 <- matrix(0, nrow = n[5], ncol = p)
  df5[, 1] <- stats::runif(n[5], -5, 5)
  df5[, 2] <- df5[, 1]
  df5[, 3] <- stats::rnorm(n[5], 10, 0.03)

  if(p == 3) {

    df1 <- tibble::as_tibble(df1, .name_repair = "minimal")
    df2 <- tibble::as_tibble(df2, .name_repair = "minimal")
    df3 <- tibble::as_tibble(df3, .name_repair = "minimal")
    df4 <- tibble::as_tibble(df4, .name_repair = "minimal")
    df5 <- tibble::as_tibble(df5, .name_repair = "minimal")

    names(df1) <- paste0("x", 1:3)
    names(df2) <- paste0("x", 1:3)
    names(df3) <- paste0("x", 1:3)
    names(df4) <- paste0("x", 1:3)
    names(df5) <- paste0("x", 1:3)

    df1 <- df1 |> dplyr::mutate(cluster = "cluster1")
    df2 <- df2 |> dplyr::mutate(cluster = "cluster2")
    df3 <- df3 |> dplyr::mutate(cluster = "cluster3")
    df4 <- df4 |> dplyr::mutate(cluster = "cluster4")
    df5 <- df5 |> dplyr::mutate(cluster = "cluster5")

    df <- dplyr::bind_rows(df1, df2, df3, df4, df5)

  } else { # p >=4

    df1[, 4] <- stats::rnorm(n[1], 10, 0.03)
    df2[, 4] <- stats::rnorm(n[2], 10, 0.03)
    df3[, 4] <- stats::rnorm(n[3], 10, 0.03)
    df4[, 4] <- stats::rnorm(n[4], 10, 0.03)
    df5[, 4] <- stats::rnorm(n[5], 10, 0.03)

    df1 <- tibble::as_tibble(df1, .name_repair = "minimal")
    df2 <- tibble::as_tibble(df2, .name_repair = "minimal")
    df3 <- tibble::as_tibble(df3, .name_repair = "minimal")
    df4 <- tibble::as_tibble(df4, .name_repair = "minimal")
    df5 <- tibble::as_tibble(df5, .name_repair = "minimal")

    names(df1) <- paste0("x", 1:4)
    names(df2) <- paste0("x", 1:4)
    names(df3) <- paste0("x", 1:4)
    names(df4) <- paste0("x", 1:4)
    names(df5) <- paste0("x", 1:4)

    df1 <- df1 |> dplyr::mutate(cluster = "cluster1")
    df2 <- df2 |> dplyr::mutate(cluster = "cluster2")
    df3 <- df3 |> dplyr::mutate(cluster = "cluster3")
    df4 <- df4 |> dplyr::mutate(cluster = "cluster4")
    df5 <- df5 |> dplyr::mutate(cluster = "cluster5")

    df <- dplyr::bind_rows(df1, df2, df3, df4, df5)

    if (p > 4) {

      noise_df <- gen_noisedims(n = NROW(df), p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4))
      names(noise_df) <- paste0("x", 5:p)

      df <- dplyr::bind_cols(df, noise_df) |>
        dplyr::select(dplyr::starts_with("x"), "cluster")

    }

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
#' seven_branching_data <- gen_sevenbranches(n = c(200, 100, 250, 300, 150, 400, 50), p = 4)
gen_sevenbranches <- function(n = c(200, 100, 250, 300, 150, 400, 50), p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")

  }

  if (length(n) != 7) {
     cli::cli_abort("n should contain exactly 7 values.")
  }

  if (any(n < 0)) {
     cli::cli_abort("Values in n should be positive.")
  }

  df1 <- matrix(0, nrow = n[1], ncol = p)
  df1[, 1] <- stats::runif(n[1], -2, 2)
  df1[, 2] <- -(df1[, 1]^3 + stats::runif(n[1], 0, 1)) + stats::runif(n[1], 0, 0.2)
  df1[, 3] <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)

  df2 <- matrix(0, nrow = n[2], ncol = p)
  df2[, 1] <- stats::runif(n[2], -2, 1.5)
  df2[, 2] <- (df2[, 1]^3 + stats::runif(n[2], 0, 1)) + stats::runif(n[2], 0, 0.2)
  df2[, 3] <- rep(0, n[2]) + stats::rnorm(n[2], 10, 0.03)

  df3 <- matrix(0, nrow = n[3], ncol = p)
  df3[, 1] <- stats::runif(n[3], -2, 1.5)
  df3[, 2] <- (1 + (df3[, 1] - 3)^2 + stats::runif(n[3], 0, 1)) + stats::runif(n[3], 0, 0.1)
  df3[, 3] <- rep(0, n[3]) + stats::rnorm(n[3], 10, 0.03)

  df4 <- matrix(0, nrow = n[4], ncol = p)
  df4[, 1] <- stats::runif(n[4], -0.5, 3)
  df4[, 2] <- (1 + -(df4[, 1] - 3)^2 + stats::runif(n[4], 0, 1)) + stats::runif(n[4], 0, 0.1)
  df4[, 3] <- rep(0, n[4]) + stats::rnorm(n[4], 10, 0.03)

  df5 <- matrix(0, nrow = n[5], ncol = p)
  df5[, 1] <- stats::runif(n[5], -1, 1)
  df5[, 2] <- (20 + df5[, 1]^3 + stats::runif(n[5], 0, 0.1)) + stats::runif(n[5], 0, 0.01)
  df5[, 3] <- rep(0, n[5]) + stats::rnorm(n[5], 10, 0.03)

  df6 <- matrix(0, nrow = n[6], ncol = p)
  df6[, 1] <- stats::runif(n[6], -2, 2)
  df6[, 2] <- (df6[, 1]^2 + stats::runif(n[6], 0, 0.1)) + stats::runif(n[6], 0, 0.01) + 10
  df6[, 3] <- rep(0, n[6]) + stats::rnorm(n[6], 10, 0.03)

  df7 <- matrix(0, nrow = n[7], ncol = p)
  df7[, 1] <- stats::runif(n[7], -2, 2)
  df7[, 2] <- (df7[, 1]^2 + stats::runif(n[7], 0, 0.2)) + stats::runif(n[7], 0, 0.01) + 15
  df7[, 3] <- rep(0, n[7]) + stats::rnorm(n[7], 10, 0.03)

  if(p == 3) {

    df1 <- tibble::as_tibble(df1, .name_repair = "minimal")
    df2 <- tibble::as_tibble(df2, .name_repair = "minimal")
    df3 <- tibble::as_tibble(df3, .name_repair = "minimal")
    df4 <- tibble::as_tibble(df4, .name_repair = "minimal")
    df5 <- tibble::as_tibble(df5, .name_repair = "minimal")
    df6 <- tibble::as_tibble(df6, .name_repair = "minimal")
    df7 <- tibble::as_tibble(df7, .name_repair = "minimal")

    names(df1) <- paste0("x", 1:3)
    names(df2) <- paste0("x", 1:3)
    names(df3) <- paste0("x", 1:3)
    names(df4) <- paste0("x", 1:3)
    names(df5) <- paste0("x", 1:3)
    names(df6) <- paste0("x", 1:3)
    names(df7) <- paste0("x", 1:3)

    df1 <- df1 |> dplyr::mutate(cluster = "cluster1")
    df2 <- df2 |> dplyr::mutate(cluster = "cluster2")
    df3 <- df3 |> dplyr::mutate(cluster = "cluster3")
    df4 <- df4 |> dplyr::mutate(cluster = "cluster4")
    df5 <- df5 |> dplyr::mutate(cluster = "cluster5")
    df6 <- df6 |> dplyr::mutate(cluster = "cluster6")
    df7 <- df7 |> dplyr::mutate(cluster = "cluster7")

    df <- dplyr::bind_rows(df1, df2, df3, df4, df5, df6, df7)

  } else { # p >=4

    df1[, 4] <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)
    df2[, 4] <- rep(0, n[2]) - stats::rnorm(n[2], 10, 0.03)
    df3[, 4] <- rep(0, n[3]) - stats::rnorm(n[3], 10, 0.03)
    df4[, 4] <- rep(0, n[4]) - stats::rnorm(n[4], 10, 0.03)
    df5[, 4] <- rep(0, n[5]) - stats::rnorm(n[5], 10, 0.03)
    df6[, 4] <- rep(0, n[6]) - stats::rnorm(n[6], 10, 0.03)
    df7[, 4] <- rep(0, n[7]) - stats::rnorm(n[7], 10, 0.03)

    df1 <- tibble::as_tibble(df1, .name_repair = "minimal")
    df2 <- tibble::as_tibble(df2, .name_repair = "minimal")
    df3 <- tibble::as_tibble(df3, .name_repair = "minimal")
    df4 <- tibble::as_tibble(df4, .name_repair = "minimal")
    df5 <- tibble::as_tibble(df5, .name_repair = "minimal")
    df6 <- tibble::as_tibble(df6, .name_repair = "minimal")
    df7 <- tibble::as_tibble(df7, .name_repair = "minimal")

    names(df1) <- paste0("x", 1:4)
    names(df2) <- paste0("x", 1:4)
    names(df3) <- paste0("x", 1:4)
    names(df4) <- paste0("x", 1:4)
    names(df5) <- paste0("x", 1:4)
    names(df6) <- paste0("x", 1:4)
    names(df7) <- paste0("x", 1:4)

    df1 <- df1 |> dplyr::mutate(cluster = "cluster1")
    df2 <- df2 |> dplyr::mutate(cluster = "cluster2")
    df3 <- df3 |> dplyr::mutate(cluster = "cluster3")
    df4 <- df4 |> dplyr::mutate(cluster = "cluster4")
    df5 <- df5 |> dplyr::mutate(cluster = "cluster5")
    df6 <- df6 |> dplyr::mutate(cluster = "cluster6")
    df7 <- df7 |> dplyr::mutate(cluster = "cluster7")

    df <- dplyr::bind_rows(df1, df2, df3, df4, df5, df6, df7)

    if (p > 4) {

      noise_df <- gen_noisedims(n = NROW(df), p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4))
      names(noise_df) <- paste0("x", 5:p)

      df <- dplyr::bind_cols(df, noise_df) |>
        dplyr::select(dplyr::starts_with("x"), "cluster")

    }

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
#' four_branching_data <- gen_fourbranchesBkg(n = c(200, 300, 150, 250), p = 4)
gen_fourbranchesBkg <- function(n = c(200, 300, 150, 250), p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")

  }

  if (length(n) != 4) {
     cli::cli_abort("n should contain exactly 4 values.")
  }

  if (any(n < 0)) {
     cli::cli_abort("Values in n should be positive.")
  }

  df1 <- matrix(0, nrow = n[1], ncol = p)
  df1[, 1] <- stats::runif(n[1], -5, 1)
  df1[, 2] <- (exp(df1[, 1]) + stats::runif(n[1], 0, 0.1)) + stats::runif(n[1], 0, 0.2)
  df1[, 3] <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)

  df2 <- matrix(0, nrow = n[2], ncol = p)
  df2[, 1] <- stats::runif(n[2], -1, 5)
  df2[, 2] <- (exp(-df2[, 1]) + stats::runif(n[2], 0, 0.1)) + stats::runif(n[2], 0, 0.2)
  df2[, 3] <- rep(0, n[2]) + stats::rnorm(n[2], 10, 0.03)

  df3 <- matrix(0, nrow = n[3], ncol = p)
  df3[, 1] <- stats::runif(n[3], 0, 5)
  df3[, 2] <- (log(df3[, 1]) + stats::runif(n[3], 0, 0.1)) + stats::runif(n[3], 0, 0.2)
  df3[, 3] <- rep(0, n[3]) + stats::rnorm(n[3], 10, 0.03)

  df4 <- matrix(0, nrow = n[4], ncol = p)
  df4[, 1] <- stats::runif(n[4], -5, 0)
  df4[, 2] <- (log(-df4[, 1]) + stats::runif(n[4], 0, 0.1)) + stats::runif(n[4], 0, 0.2)
  df4[, 3] <- rep(0, n[4]) + stats::rnorm(n[4], 10, 0.03)

  df5 <- matrix(0, nrow = sum(n) * 0.1, ncol = p)
  df5[, 1] <- stats::runif(sum(n) * 0.1, -5, 0)
  df5[, 2] <- stats::runif(sum(n) * 0.1, 0, 0.8) + stats::runif(sum(n) * 0.1, 0, 0.8)
  df5[, 3] <- rep(0, sum(n) * 0.1) + stats::rnorm(sum(n) * 0.1, 10, 0.03)

  if(p == 3) {

    df1 <- tibble::as_tibble(df1, .name_repair = "minimal")
    df2 <- tibble::as_tibble(df2, .name_repair = "minimal")
    df3 <- tibble::as_tibble(df3, .name_repair = "minimal")
    df4 <- tibble::as_tibble(df4, .name_repair = "minimal")
    df5 <- tibble::as_tibble(df5, .name_repair = "minimal")

    names(df1) <- paste0("x", 1:3)
    names(df2) <- paste0("x", 1:3)
    names(df3) <- paste0("x", 1:3)
    names(df4) <- paste0("x", 1:3)
    names(df5) <- paste0("x", 1:3)

    df1 <- df1 |> dplyr::mutate(cluster = "cluster1")
    df2 <- df2 |> dplyr::mutate(cluster = "cluster2")
    df3 <- df3 |> dplyr::mutate(cluster = "cluster3")
    df4 <- df4 |> dplyr::mutate(cluster = "cluster4")
    df5 <- df5 |> dplyr::mutate(cluster = "bkg_noise")

    df <- dplyr::bind_rows(df1, df2, df3, df4, df5)

  } else { # p >=4

    df1[, 4] <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)
    df2[, 4] <- rep(0, n[2]) - stats::rnorm(n[2], 10, 0.03)
    df3[, 4] <- rep(0, n[3]) - stats::rnorm(n[3], 10, 0.03)
    df4[, 4] <- rep(0, n[4]) - stats::rnorm(n[4], 10, 0.03)
    df5[, 4] <- rep(0, sum(n) * 0.1) - stats::rnorm(sum(n) * 0.1, 10, 0.03)

    df1 <- tibble::as_tibble(df1, .name_repair = "minimal")
    df2 <- tibble::as_tibble(df2, .name_repair = "minimal")
    df3 <- tibble::as_tibble(df3, .name_repair = "minimal")
    df4 <- tibble::as_tibble(df4, .name_repair = "minimal")
    df5 <- tibble::as_tibble(df5, .name_repair = "minimal")

    names(df1) <- paste0("x", 1:4)
    names(df2) <- paste0("x", 1:4)
    names(df3) <- paste0("x", 1:4)
    names(df4) <- paste0("x", 1:4)
    names(df5) <- paste0("x", 1:4)

    df1 <- df1 |> dplyr::mutate(cluster = "cluster1")
    df2 <- df2 |> dplyr::mutate(cluster = "cluster2")
    df3 <- df3 |> dplyr::mutate(cluster = "cluster3")
    df4 <- df4 |> dplyr::mutate(cluster = "cluster4")
    df5 <- df5 |> dplyr::mutate(cluster = "bkg_noise")

    df <- dplyr::bind_rows(df1, df2, df3, df4, df5)

    if (p > 4) {

      noise_df <- gen_noisedims(n = NROW(df), p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4))
      names(noise_df) <- paste0("x", 5:p)

      df <- dplyr::bind_cols(df, noise_df) |>
        dplyr::select(dplyr::starts_with("x"), "cluster")

    }

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

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")

  }

  if (length(n) != 8) {
     cli::cli_abort("n should contain exactly 5 values.")
  }

  if (any(n < 0)) {
     cli::cli_abort("Values in n should be positive.")
  }

  df1 <- matrix(0, nrow = n[1], ncol = p)
  df1[, 1] <- stats::runif(n[1], -1, 2)
  df1[, 2] <- (exp(df1[, 1]) + stats::runif(n[1], 0, 0.1)) + stats::runif(n[1])
  df1[, 3] <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)

  df2 <- matrix(0, nrow = n[2], ncol = p)
  df2[, 1] <- stats::runif(n[2], -1, 1)
  df2[, 2] <- (exp(2 * df2[, 1]) + stats::runif(n[2], 0, 0.1)) + stats::runif(n[2], 0, 0.2)
  df2[, 3] <- rep(0, n[2]) + stats::rnorm(n[2], 10, 0.03)

  df3 <- matrix(0, nrow = n[3], ncol = p)
  df3[, 1] <- stats::runif(n[3], -1, 0.6)
  df3[, 2] <- (exp(3 * df3[, 1]) + stats::runif(n[3], 0, 0.1)) + stats::runif(n[3], 0, 0.2)
  df3[, 3] <- rep(0, n[3]) + stats::rnorm(n[3], 10, 0.03)

  df4 <- matrix(0, nrow = n[4], ncol = p)
  df4[, 1] <- stats::runif(n[4], -1, 3)
  df4[, 2] <- (exp(0.5 * df4[, 1]) + stats::runif(n[4], 0, 0.1)) + stats::runif(n[4], 0, 0.2)
  df4[, 3] <- rep(0, n[4]) + stats::rnorm(n[4], 10, 0.03)

  df5 <- matrix(0, nrow = n[5], ncol = p)
  df5[, 1] <- stats::runif(n[5], -2, 1)
  df5[, 2] <- (exp(-df5[, 1]) + stats::runif(n[5], 0, 0.1)) + stats::runif(n[5], 0, 0.2)
  df5[, 3] <- rep(0, n[5]) + stats::rnorm(n[5], 10, 0.03)

  df6 <- matrix(0, nrow = n[6], ncol = p)
  df6[, 1] <- stats::runif(n[6], -1, 1)
  df6[, 2] <- (exp(2 * -df6[, 1]) + stats::runif(n[6], 0, 0.1)) + stats::runif(n[6], 0, 0.2)
  df6[, 3] <- rep(0, n[6]) + stats::rnorm(n[6], 10, 0.03)

  df7 <- matrix(0, nrow = n[7], ncol = p)
  df7[, 1] <- stats::runif(n[7], -0.6, 1)
  df7[, 2] <- (exp(3 * -df7[, 1]) + stats::runif(n[7], 0, 0.1)) + stats::runif(n[7], 0, 0.2)
  df7[, 3] <- rep(0, n[7]) + stats::rnorm(n[7], 10, 0.03)

  df8 <- matrix(0, nrow = n[8], ncol = p)
  df8[, 1] <- stats::runif(n[8], -3, 1)
  df8[, 2] <- (exp(0.5 * -df8[, 1]) + stats::runif(n[8], 0, 0.1)) + stats::runif(n[8], 0, 0.2)
  df8[, 3] <- rep(0, n[8]) + stats::rnorm(n[8], 10, 0.03)

  if(p == 3) {

    df1 <- tibble::as_tibble(df1, .name_repair = "minimal")
    df2 <- tibble::as_tibble(df2, .name_repair = "minimal")
    df3 <- tibble::as_tibble(df3, .name_repair = "minimal")
    df4 <- tibble::as_tibble(df4, .name_repair = "minimal")
    df5 <- tibble::as_tibble(df5, .name_repair = "minimal")
    df6 <- tibble::as_tibble(df6, .name_repair = "minimal")
    df7 <- tibble::as_tibble(df7, .name_repair = "minimal")
    df8 <- tibble::as_tibble(df8, .name_repair = "minimal")

    names(df1) <- paste0("x", 1:3)
    names(df2) <- paste0("x", 1:3)
    names(df3) <- paste0("x", 1:3)
    names(df4) <- paste0("x", 1:3)
    names(df5) <- paste0("x", 1:3)
    names(df6) <- paste0("x", 1:3)
    names(df7) <- paste0("x", 1:3)
    names(df8) <- paste0("x", 1:3)

    df1 <- df1 |> dplyr::mutate(cluster = "cluster1")
    df2 <- df2 |> dplyr::mutate(cluster = "cluster2")
    df3 <- df3 |> dplyr::mutate(cluster = "cluster3")
    df4 <- df4 |> dplyr::mutate(cluster = "cluster4")
    df5 <- df5 |> dplyr::mutate(cluster = "cluster5")
    df6 <- df6 |> dplyr::mutate(cluster = "cluster6")
    df7 <- df7 |> dplyr::mutate(cluster = "cluster7")
    df8 <- df8 |> dplyr::mutate(cluster = "cluster8")

    df <- dplyr::bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)

  } else { # p >=4

    df1[, 4] <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)
    df2[, 4] <- rep(0, n[2]) - stats::rnorm(n[2], 10, 0.03)
    df3[, 4] <- rep(0, n[3]) - stats::rnorm(n[3], 10, 0.03)
    df4[, 4] <- rep(0, n[4]) - stats::rnorm(n[4], 10, 0.03)
    df5[, 4] <- rep(0, n[5]) - stats::rnorm(n[5], 10, 0.03)
    df6[, 4] <- rep(0, n[6]) - stats::rnorm(n[6], 10, 0.03)
    df7[, 4] <- rep(0, n[7]) - stats::rnorm(n[7], 10, 0.03)
    df8[, 4] <- rep(0, n[8]) - stats::rnorm(n[8], 10, 0.03)

    df1 <- tibble::as_tibble(df1, .name_repair = "minimal")
    df2 <- tibble::as_tibble(df2, .name_repair = "minimal")
    df3 <- tibble::as_tibble(df3, .name_repair = "minimal")
    df4 <- tibble::as_tibble(df4, .name_repair = "minimal")
    df5 <- tibble::as_tibble(df5, .name_repair = "minimal")
    df6 <- tibble::as_tibble(df6, .name_repair = "minimal")
    df7 <- tibble::as_tibble(df7, .name_repair = "minimal")
    df8 <- tibble::as_tibble(df8, .name_repair = "minimal")

    names(df1) <- paste0("x", 1:4)
    names(df2) <- paste0("x", 1:4)
    names(df3) <- paste0("x", 1:4)
    names(df4) <- paste0("x", 1:4)
    names(df5) <- paste0("x", 1:4)
    names(df6) <- paste0("x", 1:4)
    names(df7) <- paste0("x", 1:4)
    names(df8) <- paste0("x", 1:4)

    df1 <- df1 |> dplyr::mutate(cluster = "cluster1")
    df2 <- df2 |> dplyr::mutate(cluster = "cluster2")
    df3 <- df3 |> dplyr::mutate(cluster = "cluster3")
    df4 <- df4 |> dplyr::mutate(cluster = "cluster4")
    df5 <- df5 |> dplyr::mutate(cluster = "cluster5")
    df6 <- df6 |> dplyr::mutate(cluster = "cluster6")
    df7 <- df7 |> dplyr::mutate(cluster = "cluster7")
    df8 <- df8 |> dplyr::mutate(cluster = "cluster8")

    df <- dplyr::bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)

    if (p > 4) {

      noise_df <- gen_noisedims(n = NROW(df), p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4))
      names(noise_df) <- paste0("x", 5:p)

      df <- dplyr::bind_cols(df, noise_df) |>
        dplyr::select(dplyr::starts_with("x"), "cluster")

    }

  }


  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

gen_expbranches <- function(n = 400, p = 4, k = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (k <= 0) {
    cli::cli_abort("k should be positive.")
  }

  n_vec <- rep(n/k, k)

  scale_vec <- sample(seq(0.5, 2, by = 0.1), size = k)

  df <- matrix(0, nrow = n, ncol = p)

  for (i in 1:k) {

    df1 <- matrix(0, nrow = n_vec[i], ncol = 2)

    # gen the core curvilinear pattern in 2D
    df1[, 1] <- stats::runif(n_vec[i], -2, 2)
    df1[, 2] <- exp(scale_vec[i] * df1[, 1]) + stats::runif(n_vec[i], 0, 0.1)

    if (i %% 2 != 0) {
      # i is odd
      df1[, 2]< - -1 * df1[, 2] # To generate mirror pattern
    }

    if (p > 2){

      noise_df <- gen_noisedims(n = n_vec[i], p = (p-2), m = rep(0, p-2), s = rep(0.1, p-2)) |>
        as.matrix()
      colnames(noise_df) <- paste0("x", 3:p)

      df1 <- cbind(df1, noise_df)

    }

    df <- rbind(df, df1)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}
