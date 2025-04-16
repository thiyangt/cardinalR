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
    noise_bkg[[i]] <- stats::rnorm(n, mean = m[i], sd = s[i])
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

#' Generates a vector of positive integers whose product is approximately equal to a target value.
#'
#' This function takes a target integer `n` and the number of dimensions `p`,
#' and returns a vector `n_vec` of length `p` containing positive integers.
#' The goal is to have the product of the elements in `n_vec` be as close as
#' possible to `n`, especially when `n` is not a perfect p-th power.
#'
#' @param n The target positive integer value for the product of the output vector.
#' @param p The number of dimensions (the length of the output vector). Must be a positive integer.
#' @return A sorted vector of positive integers of length `p`. The product of the elements
#'         in this vector will be approximately equal to `n`. If `n` is a perfect
#'         p-th power, the elements will be equal.
#' @examples
#' gen_nproduct(500, 6) # Example with n=500, p=6
#' gen_nproduct(700, 4) # Example with n=700, p=4
#' gen_nproduct(625, 4) # Example with n=625 (perfect power)
#' gen_nproduct(30, 3)  # Example with n=30, p=3
#' gen_nproduct(7, 2)   # Example where exact product might be hard
gen_nproduct <- function(n = 500, p = 4) {

  if (p <= 0 || !is.numeric(p) || !is.numeric(n) || n <= 0 || p != round(p)) {
    cli::cli_abort("n must be a positive number, and p must be a positive integer.")
  }

  n_vec <- rep(round(n^(1/p)), p)
  current_product <- prod(n_vec)

  max_iterations <- 1000 # Safety limit

  for (i in 1:max_iterations) {
    if (abs(current_product - n) <= max(1, n * 0.01)) {
      break
    }

    if (current_product < n) {
      idx_increase <- sample(1:p, 1)
      n_vec[idx_increase] <- n_vec[idx_increase] + 1
    } else {
      idx_decrease <- sample(1:p, 1)
      if (n_vec[idx_decrease] > 1) {
        n_vec[idx_decrease] <- n_vec[idx_decrease] - 1
      }
    }
    current_product <- prod(n_vec)
  }

  return(sort(n_vec))
}

#' Generates a vector of positive integers whose summation is approximately equal to a target value.
#'
#' This function takes a target integer `n` and the number of clusters `k`,
#' and returns a vector `n_vec` of length `k` containing positive integers.
#' The goal is to have the summation of the elements in `n_vec` be as close as
#' possible to `n`, especially when `n` is not a perfect multiplier of `k`.
#'
#' @param n The target positive integer value for the summation of the output vector.
#' @param p The number of dimensions (the length of the output vector). Must be a positive integer.
#' @return A sorted vector of positive integers of length `k`. The summation of the elements
#'         in this vector will be approximately equal to `n`. If `n` is a perfectly
#'         divisible by `k`, the elements will be equal.
#' @examples
#' gen_nsum(500, 6) # Example with n=500, p=6
#' gen_nsum(700, 4) # Example with n=700, p=4
#' gen_nsum(625, 5) # Example with n=625 (perfect division)
#' gen_nsum(30, 3)  # Example with n=30, p=3
gen_nsum <- function(n = 500, k = 4) {
  if (!is.numeric(n) || n <= 0 || !is.numeric(k) || k <= 0 || k != round(k)) {
    stop("n must be a positive number, and k must be a positive integer.")
  }

  base_size <- floor(n / k)
  remainder <- n %% k
  n_vec <- rep(base_size, k)

  # Distribute the remainder among the clusters to balance the sizes
  if (remainder > 0) {
    indices_to_add <- sample(1:k, remainder, replace = FALSE)
    n_vec[indices_to_add] <- n_vec[indices_to_add] + 1
  }

  return(n_vec)
}

utils::globalVariables(c("n"))
