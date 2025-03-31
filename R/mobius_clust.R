#' Generate Mobius Cluster with Noise
#'
#' This function generates a dataset consisting of a mobius cluster with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the mobius cluster with added noise.
#' @export
#'
#' @examples
#' mobius_cluster <- mobius_clust(
#'   n = 200, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
mobius_clust <- function(n, num_noise, min_n, max_n) {
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

  df1 <- mobius_5d(n = n * 0.80, num_noise = 0)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df1)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df1 <- cbind(df1, noise_mat)
  }

  ## To add background noise
  df2 <- gen_bkg_noise(n = n * 0.20, num_dims = NCOL(df1), mean = 0, sd = 0.3)
  df <- rbind(df1, df2)
  df
}
