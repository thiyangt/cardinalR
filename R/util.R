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
#' gen_noise_dims(n = 50, num_noise = 3, min_n = -0.01, max_n = 0.01)
#'
#' @export
gen_noise_dims <- function(n, num_noise, min_n, max_n) {
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

  ## Is there are noise dimensions?
  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    # Initialize an empty list to store the vectors
    noise_dim_val_list <- list()

    for (j in 1:num_noise) {
      if ((j %% 2) == 0) {
        noise_dim_val_list[[j]] <- stats::runif(n, min = min_n, max = max_n)
      } else {
        noise_dim_val_list[[j]] <- (-1) * stats::runif(n, min = min_n, max = max_n)
      }
    }

    noise_mat <- matrix(unlist(noise_dim_val_list), ncol = num_noise)
  } else {
    noise_mat <- NULL
  }

  return(noise_mat)
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
#' gen_bkg_noise(n = 50, num_dims = 3, mean = 5, sd = 2)
#'
#' @export
gen_bkg_noise <- function(n, num_dims, mean, sd) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_dims < 0) {
    stop("Number of dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_dims)) {
    stop("Missing num_noise.")
  }

  if (num_dims != 0) {
    if (missing(mean)) {
      stop("Missing mean.")
    }

    if (missing(sd)) {
      stop("Missing sd.")
    }

    # Initialize an empty list to store the vectors
    noise_bkg_val_list <- list()

    for (j in 1:num_dims) {
      noise_bkg_val_list[[j]] <- stats::rnorm(n, mean = mean, sd = sd)
    }

    names(noise_bkg_val_list) <- paste0("x", 1:length(noise_bkg_val_list))

    bkg_mat <- tibble::as_tibble(noise_bkg_val_list)
  } else {
    bkg_mat <- NULL
  }

  return(bkg_mat)
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
