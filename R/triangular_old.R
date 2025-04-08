#' Generate p-D Triangular Prism
#'
#' This function generates p-D triangular prism datasets.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a triangular prism in p-D.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' triangular_data <- gen_tri_prism(n = 300, p = 3)
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


#' Generate Triangular Prism clusters
#'
#' This function generates p-D triangular prism clusters datasets.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 3) representing the number of clusters.
#' @return A data containing a triangular prism clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' triangular_clusts_data <- gen_clusts_tri(n = c(200, 300, 500), p = 4, k = 3)
gen_clusts_tri <- function(n = c(200, 300, 500), p = 4, k = 3) {

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

  ## To generate the shifting vector
  shift_vec <- stats::runif(k, -1, 1)

  df <- tibble::tibble()

  for (j in 1:k) {

    trace_point <- stats::runif(p)
    corner_points <- geozoo::simplex(p=p)$points

    data_list <- lapply(1:p, function(i) rep(0, n[j]))
    names(data_list) <- paste0("x", 1:p)

    df1 <- tibble::tibble(!!!data_list)

    for (i in 1:n[j]) {
      trace_point <- (corner_points[sample((p + 1), 1), ] + trace_point) / 2
      df1[i, ] <- as.list(trace_point)
    }

    df1 <- shift_vec[j] * df1

    df1 <- df1 |>
      dplyr::mutate(cluster = paste0("cluster", j))

    df <- dplyr::bind_rows(df, df1)

  }

  ## To swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
