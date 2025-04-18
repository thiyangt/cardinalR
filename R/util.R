#' Generate Random Noise Dimensions
#'
#' This function generates random noise dimensions to be added to the coordinates of a data structure.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param m A numeric vector (default: c(0, 0, 0, 0)) representing the mean along each dimensions.
#' @param s A numeric vector (default: c(2, 2, 2, 2)) representing the standard deviation along each dimensions.
#'
#' @return A data containing the generated random noise dimensions.
#'
#' @examples
#' set.seed(20240412)
#' gen_noisedims(n = 500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2))
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
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param m A numeric vector (default: c(0, 0, 0, 0)) representing the mean along each dimensions.
#' @param s A numeric vector (default: c(2, 2, 2, 2)) representing the standard deviation along each dimensions.
#'
#' @return A data containing the generated background noise data.
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
#' @export
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
#' @export
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

#' Generate Random Noise Dimensions With Wavy Pattern
#'
#' This function generates random noise dimensions by adding wavy patterns.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param theta A numeric vector representing the nonlinearity along each dimensions.
#'
#' @return A data containing the generated random noise dimensions.
#'
#' @examples
#' set.seed(20240412)
#' gen_wavydims1(n = 500, p = 4, theta = seq(pi / 6, 12 * pi / 6, length.out = 500))
#'
#' @export
gen_wavydims1 <- function(n = 500, p = 4, theta = seq(pi / 6, 12 * pi / 6, length.out = 500)) {

  if (length(theta) != n) {
    cli::cli_abort("The length of theta should be {.val {n}}.")
  }

  # Initialize an empty list to store the vectors
  wavy_df <- list()
  scale_vec <- sample(seq(1, 4, by = 0.3), size = p, replace = TRUE)

  for (i in 1:p) {
    wavy_df[[i]] <- scale_vec[i] * theta + stats::rnorm(n, 0, 0.5)
  }

  df <- tibble::as_tibble(wavy_df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Wavy shaped noise dimensions generation completed successfully! 🎉")
  return(df)

}

#' Generate Random Noise Dimensions With Wavy Pattern
#'
#' This function generates random noise dimensions by adding wavy patterns.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param x1_vec A numeric vector representing the first dimension of the data structure.
#'
#' @return A data containing the generated random noise dimensions.
#'
#' @examples
#' set.seed(20240412)
#' theta <- seq(0, spins * 2 * pi, length.out = n)
#' x1 <- sin(phi) * cos(theta)
#' gen_wavydims2(n = 500, p = 4, x1_vec = x1)
#'
#' @export
gen_wavydims2 <- function(n = 500, p = 4, x1_vec) {

  if (length(x1_vec) != n) {
    cli::cli_abort("The length of x1_vec should be {.val {n}}.")
  }

  # Initialize an empty list to store the vectors
  wavy_df <- list()

  for (i in 1:p) {

    # Introduce non-linearity based on x1 and add random noise
    # You can experiment with different non-linear functions and noise levels
    power <- sample(2:5, 1) # Random power for the polynomial
    scale_factor <- stats::runif(1, 0.5, 2) # Random scaling
    noise_level <- stats::runif(1, 0, 0.05)

    wavy_df[[i]] <- scale_factor * ((-1)^(i %/% 2)) * (x1_vec^power) + stats::runif(n, -noise_level, noise_level * 2)

  }

  df <- tibble::as_tibble(wavy_df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Wavy shaped noise dimensions generation completed successfully! 🎉")
  return(df)

}

#' Generate Random Noise Dimensions With Wavy Pattern
#'
#' This function generates random noise dimensions by adding wavy patterns.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param data A matrix representing the first three dimensions of the data structure.
#'
#' @return A data containing the generated random noise dimensions.
#'
#' @examples
#' set.seed(20240412)
#' df <- gen_scurve(n = 500, p = 4) |> as.matrix()
#' gen_wavydims3(n = 500, p = 4, data = df)
#'
#' @export
gen_wavydims3 <- function(n = 500, p = 4, data) {

  noise_level <- 0.05
  scaling_factor <- 0.2

  x1 <- data[, 1]
  x2 <- data[, 2]
  x3 <- data[, 3]

  df <- matrix(0, nrow = n, ncol = p)

  for (i in 1:p) {
    # Strategy 1 & 2: Small variations around existing dimensions
    if (i == 1) df[, i] <- x1 + scaling_factor * stats::runif(n, -noise_level, noise_level)
    if (i == 2) df[, i] <- x2 + scaling_factor * stats::runif(n, -noise_level, noise_level)
    if (i == 3) df[, i] <- x3 + scaling_factor * stats::runif(n, -noise_level, noise_level)
    # Strategy 3: Non-linear transformations with small scaling
    if (i > 4) {
      if (i %% 3 == 1) df[, i] <- x1^2 * scaling_factor * noise_level + stats::runif(n, -noise_level * 0.5, noise_level * 0.5)
      if (i %% 3 == 2) df[, i] <- x2 * x3 * scaling_factor * noise_level + stats::runif(n, -noise_level * 0.5, noise_level * 0.5)
      if (i %% 3 == 0) df[, i] <- sin(x1 + x3) * scaling_factor * noise_level + stats::runif(n, -noise_level * 0.5, noise_level * 0.5)
    }
  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Wavy shaped noise dimensions generation completed successfully! 🎉")
  return(df)

}

#' Generate Rotations
#'
#' This function generates a rotation matrix.
#'
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param planes_angles A numeric list which contains plane and the corresponding angle along that plane.
#'
#' @return A matrix containing the rotations.
#'
#' @examples
#' set.seed(20240412)
#' rotations_4d <- list(
#' list(plane = c(1, 2), angle = 60), # Rotation in the (1, 2) plane
#' list(plane = c(3, 4), angle = 90)  # Rotation in the (3, 4) plane
#' )
#' gen_rotation(planes_angles = rotations_4d)
#'
#' @export
gen_rotation <- function(p = 4, planes_angles) {
  if (!is.list(planes_angles) || length(planes_angles) == 0) {
    cli::cli_abort("The 'planes_angles' argument must be a non-empty list.")
  }

  # Initialize an p x p identity matrix
  rotation_matrix <- diag(p)

  for (item in planes_angles) {
    if (!is.list(item) || length(item) != 2 || length(item[[1]]) != 2 ||
        !all(item[[1]] >= 1 & item[[1]] <= p) || item[[1]][1] == item[[1]][2] ||
        !is.numeric(item[[2]]) || length(item[[2]]) != 1) {
      cli::cli_abort("Each item in 'planes_angles' must be a list containing a plane (vector of two distinct axis indices) and an angle (numeric in degrees).")
    }

    plane <- item[[1]]
    angle_degrees <- item[[2]]
    angle_radians <- angle_degrees * pi / 180
    cos_theta <- cos(angle_radians)
    sin_theta <- sin(angle_radians)

    # Get the indices of the rotation plane
    i <- plane[1]
    j <- plane[2]

    # Create a rotation matrix for the current plane
    plane_rotation_matrix <- diag(p)
    plane_rotation_matrix[i, i] <- cos_theta
    plane_rotation_matrix[j, j] <- cos_theta
    plane_rotation_matrix[i, j] <- -sin_theta
    plane_rotation_matrix[j, i] <- sin_theta

    # Multiply the overall rotation matrix by the current plane's rotation matrix
    rotation_matrix <- plane_rotation_matrix %*% rotation_matrix
  }

  return(rotation_matrix)
}

utils::globalVariables(c("n"))
