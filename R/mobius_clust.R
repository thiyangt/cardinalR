#' Generate Gaussian cluster with the Mobius Cluster
#'
#' This function generates a dataset consisting of a mobius cluster and Gaussian cluster.
#'
#' @param n A numeric vector (default: c(200, 100)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the mobius cluster and Gaussian cluster.
#' @export
#'
#' @examples
#' mobius_cluster <- mobius_gau_clust(n = c(200, 100), p = 4)
mobius_gau_clust <- function(n = c(200, 100), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df1 <- geozoo::mobius(n = n[1], p = p)$points |>
    tibble::as_tibble(.name_repair = "unique")

  names(df1) <- paste0("x", 1:p)

  ## To add background noise
  df2 <- gen_bkg_noise(n = n[2], num_dims = NCOL(df1), mean = 0, sd = 0.1)
  df <- dplyr::bind_rows(df1, df2)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
