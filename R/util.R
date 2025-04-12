#' Generate Random Noise Dimensions
#'
#' This function generates random noise dimensions to be added to the coordinates of a sphere.
#'
#' @param n The number of observations for which to generate noise dimensions.
#' @param num_noise The number of noise dimensions to generate.
#' @param min_n The minimum value for the random noise.
#' @param max_n The maximum value for the random noise.
#'
#' @return A matrix containing the generated random noise dimensions.
#'
#' @examples
#' # Generate random noise dimensions with 3 dimensions, minimum value -1, and maximum value 1
#' set.seed(20240412)
#' gen_noisedims(n = 50, num_noise = 3, min_n = -0.01, max_n = 0.01)
#'
#' @export
gen_noisedims <- function(n = 500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2)) {

  # Initialize an empty list to store the vectors
  noise_dim <- list()

  for (i in 1:p) {
    if ((i %% 2) == 0) {
      noise_dim[[i]] <- stats::rnorm(n, mean = m[i], sd = s[i])
    } else {
      noise_dim[[i]] <- (-1) * stats::rnorm(n, mean = m[i], sd = s[i])
    }
  }

  df <- tibble::as_tibble(noise_dim, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Noise dimensions generation completed successfully! 🎉")
  return(df)
}

#' Generate Background Noise Data
#'
#' This function generates background noise data with specified parameters such as
#' the number of samples, number of dimensions, mean, and standard deviation.
#'
#' @param n Number of samples to generate.
#' @param num_dims Number of dimensions (columns) of the data.
#' @param mean Mean of the normal distribution used to generate noise (default is 0).
#' @param sd Standard deviation of the normal distribution used to generate noise (default is 1).
#'
#' @return A matrix containing the generated background noise data, with
#' \code{n} rows and \code{num_dims} columns.
#'
#' @examples
#'
#' # Generate background noise with custom mean and standard deviation
#' set.seed(20240412)
#' gen_bkgnoise(n = 500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2))
#'
#' @export
gen_bkgnoise <- function(n = 500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2)) {

  # Initialize an empty list to store the vectors
  noise_bkg <- list()

  for (i in 1:p) {
    noise_bkg[[i]] <- stats::rnorm(n, mean = m[i], sd = s[i,])
  }

  df <- tibble::as_tibble(noise_bkg, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Background noise generation completed successfully! 🎉")
  return(df)

}

#' Randomize Rows of a Data Frame
#'
#' This function randomly shuffles the rows of a given data frame.
#'
#' @param data A data frame to be randomized.
#'
#' @return A data frame with rows randomly shuffled.
#' @export
#'
#' @examples
#' randomize_rows(mobius_clust_data)
randomize_rows <- function(data) {
  data |> dplyr::slice_sample(n = NROW(data))
}


#' Relocate Clusters in High-Dimensional Space
#'
#' This function relocates clusters in a dataset by centering each cluster
#' and shifting it based on a given transformation matrix.
#'
#' @param data A tibble or data frame containing clustered data.
#'        It must have a `cluster` column indicating cluster membership.
#' @param vert_mat A matrix specifying the translation vectors for each cluster.
#'        The number of rows must match the number of clusters.
#'
#' @return A tibble containing the relocated clusters with randomized row order.
#' @import dplyr purrr tibble
#' @export
relocate_clusters <- function(data, vert_mat) {

  data_by_clust <- data |>
    dplyr::group_split(cluster)

  relocated_data <- tibble::tibble()

  relocated_data <- purrr::map_dfr(seq_along(data_by_clust), function(i) {

    cluster_data <- data_by_clust[[i]]

    # Subtract column means
    cluster_data <- apply(cluster_data, 2, function(col) col - mean(col))

    # Relocate the cluster
    cluster_data <- as_tibble(cluster_data + matrix(rep(vert[i,], NROW(cluster_data)),
                                                    ncol = 4, byrow = TRUE))

    cluster_data
  })

  relocated_data <- randomize_rows(relocated_data)

  relocated_data

}

generate_simplex_points <- function(p, k) {
  # Generate k points in p-dimensional simplex
  # Sample k points from a (p-1)-dimensional Dirichlet distribution
  dirichlet_samples <- t(MASS::mvrnorm(n = k, mu = rep(0, p), Sigma = diag(p)))

  # Center the points to form a proper p-simplex
  simplex_points <- dirichlet_samples - rowMeans(dirichlet_samples)

  return(simplex_points)
}

utils::globalVariables(c("n"))
