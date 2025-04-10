#' Generate S-curve Data
#'
#' This function generates S-curve data.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the generated S-curve data.
#' @references
#' Buitinck, L., Louppe, G., Blondel, M., Pedregosa, F., Mueller, A.,
#' Grisel, O., ... & Varoquaux, G. (2013).
#' API design for machine learning software: experiences from the scikit-learn
#' project.
#' \emph{arXiv preprint} \emph{arXiv:1309.0238}.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_scurve(n = 500, p = 4)
#' head(data, 5)
gen_scurve <- function(n = 500, p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  noise_level <- 0.05
  scaling_factor <- 0.2

  a <- 3 * pi * stats::runif(n = n, min = -0.5, max = 0.5)
  x1 <- sin(a)
  x2 <- 2.0 * stats::runif(n = n, min = 0, max = 1)
  x3 <- sign(a) * (cos(a) - 1)

  df <- matrix(0, nrow = n, ncol = p)
  df[, 1] <- x1
  df[, 2] <- x2
  df[, 3] <- x3

  if (p > 3) {
    for (i in 4:p) {
      # Strategy 1 & 2: Small variations around existing dimensions
      if (i == 4) df[, i] <- x1 + scaling_factor * stats::runif(n, -noise_level, noise_level)
      if (i == 5) df[, i] <- x2 + scaling_factor * stats::runif(n, -noise_level, noise_level)
      if (i == 6) df[, i] <- x3 + scaling_factor * stats::runif(n, -noise_level, noise_level)
      # Strategy 3: Non-linear transformations with small scaling
      if (i > 6) {
        if (i %% 3 == 1) df[, i] <- x1^2 * scaling_factor * noise_level + stats::runif(n, -noise_level * 0.5, noise_level * 0.5)
        if (i %% 3 == 2) df[, i] <- x2 * x3 * scaling_factor * noise_level + stats::runif(n, -noise_level * 0.5, noise_level * 0.5)
        if (i %% 3 == 0) df[, i] <- sin(x1 + x3) * scaling_factor * noise_level + stats::runif(n, -noise_level * 0.5, noise_level * 0.5)
      }
    }
  }

  # Create the tibble
  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate S-curve Data with a Hole
#'
#' This function generates S-curve data with a hole by filtering out samples that
#' are not close to a specified anchor point.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the generated S-curve data with a hole.
#' @references
#' Wang, Y., Huang, H., Rudin, C., & Shaposhnik, Y. (2021).
#' Understanding how dimension reduction tools work: an empirical approach to
#' deciphering t-SNE, UMAP, TriMAP, and PaCMAP for data visualization.
#' \emph{J Mach. Learn. Res}, \emph{22}, 1-73.
#' @seealso the \href{https://github.com/YingfanWang/PaCMAP}{PaCMAP homepage}.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_scurveHole(n = 500, p = 4)
#' head(data, 5)
gen_scurveHole <- function(n = 500, p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- gen_scurve(n = n, p = 3) |>
    as.matrix()

  if (p > 3) {
    noise_df <- gen_noisedims(n = n, p = (p-3), m = rep(0, p-3), s = rep(0.5, p-3))
    df <- dplyr::bind_rows(df, noise_df)

  }

  anchor <- c(0, 1, 0)

  if ((p %% 3) == 0) {

    anchor_vec <- rep(anchor, p/3)

  } else if ((p %% 3) == 1) {

    anchor_vec <- append(rep(anchor, round(p/3)), sample(anchor, 1))

  } else { #(p %% 3) == 2

    anchor_vec <- append(rep(anchor, round(p/3)), sample(anchor, 2))

  }

  indices <- rowSums((sweep(df, 2, anchor_vec, `-`))^2) > 0.3 #0.3
  df <- df[indices, ]
  rownames(df) <- NULL

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

gen_swissroll <- function(n = 500, p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  t <- runif(n, min = 0, max = 3 * pi)  # Control parameter
  x1 <- t * cos(t)
  x2 <- t * sin(t)
  x3 <- runif(n, min = -1, max = 1)  # Adding some vertical variation

  df <- matrix(0, nrow = n, ncol = p)
  df[,1] <- x1
  df[,2] <- x2
  if (p > 2) {
    df[,3] <- x3
  }
  if (p > 3) {
    for (i in 4:p) {
      df[,i] <- sin(i * t) / i  # Additional non-linearity
    }
  }

  # Create the tibble
  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

gen_spherical_spiral_4d <- function(n = 500, p = 4, radius = 1, spiral_turns = 1) {
  if (n <= 0) {
    stop("Number of points should be a positive integer.")
  }

  # gen angles (theta, phi) for the spherical coordinates
  theta <- seq(0, spiral_turns * 2 * pi, length.out = n)  # Controls the number of spiral turns
  phi <- seq(0, pi, length.out = n)                       # Controls movement along the latitude

  df <- matrix(0, nrow = n, ncol = p)

  # Spherical to Cartesian coordinates for 4D
  df[, 1] <- radius * sin(phi) * cos(theta)
  df[, 2] <- radius * sin(phi) * sin(theta)
  df[, 3] <- radius * cos(phi) + runif(n, -0.5, 0.5)
  df[, 4] <- theta / max(theta) * radius  # Spiral along the 4th dimension

  # Extend to higher dimensions
  if (p > 4) {
    for (i in 5:d) {
      # Introduce non-linearity based on x1 and add random noise
      # You can experiment with different non-linear functions and noise levels
      power <- sample(2:5, 1) # Random power for the polynomial
      scale_factor <- stats::runif(1, 0.5, 2) # Random scaling
      noise_level <- stats::runif(1, 0, 1)

      df[, i] <- scale_factor * ((-1)^(i %/% 2)) * (x1^power) + stats::runif(n, -noise_level, noise_level * 2)
    }
  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}


# Function to gen a Helical Hyper-spiral in 4D
gen_helical_hyper_spiral_4d <- function(n = 500, p = 4, a = 0.05, b = 0.1, k = 1, spiral_radius = 0.5, scale_factor = 0.1) {
  if (n <= 0) {
    stop("Number of points should be a positive integer.")
  }

  # gen angles for the spiral (theta)
  theta <- seq(0, 5/4 * pi, length.out = n)

  df <- matrix(0, nrow = n, ncol = p)

  # Helical spiral coordinates
  df[, 1] <- spiral_radius * cos(theta)  # x1 is a circular pattern
  df[, 2] <- spiral_radius * sin(theta)  # x2 is a circular pattern
  df[, 3] <- a * theta + runif(n, -0.5, 0.5) # x3 moves linearly with theta (like a helix)
  df[, 4] <- b * sin(k * theta)          # x4 oscillates with sin(k * theta)


  # Extend to higher dimensions
  if (p > 4) {
    for (i in 5:d) {
      # Introduce non-linearity based on x1 and add random noise
      # You can experiment with different non-linear functions and noise levels
      power <- sample(2:5, 1) # Random power for the polynomial
      scale_factor <- stats::runif(1, 0.5, 2) # Random scaling
      noise_level <- stats::runif(1, 0, 1)

      df[, i] <- scale_factor * ((-1)^(i %/% 2)) * (x1^power) + stats::runif(n, -noise_level, noise_level * 2)
    }
  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

# Function to gen a conic spiral in 4D
gen_conic_spiral_4d <- function(n = 500, p = 4, spiral_turns = 1, cone_height = 2, cone_radius = 1) {

  # gen theta values to represent the angle of the spiral in the xy-plane
  theta <- seq(0, 2 * pi * spiral_turns, length.out = n)
  df <- matrix(0, nrow = n, ncol = p)

  # Spiral in the first two dimensions (x1, x2) - Archimedean spiral
  r <- cone_radius * theta  # Radius increases linearly with theta
  df[, 1] <- r * cos(theta)
  df[, 2] <- r * sin(theta)

  # Conical shape in the third dimension (x3) - linear increase with height
  df[, 3] <- cone_height * theta / max(theta) + runif(n, -0.1, 0.6) # Scaling height to range from 0 to cone_height

  # Spiral in the fourth dimension (x4) - a helical shape based on the cone
  df[, 4] <- cone_radius * sin(2 * theta) + runif(n, -0.1, 0.6) # Helical movement

  # Extend to higher dimensions
  if (p > 4) {
    for (i in 5:d) {
      # Introduce non-linearity based on x1 and add random noise
      # You can experiment with different non-linear functions and noise levels
      power <- sample(2:5, 1) # Random power for the polynomial
      scale_factor <- stats::runif(1, 0.5, 2) # Random scaling
      noise_level <- stats::runif(1, 0, 1)

      df[, i] <- scale_factor * ((-1)^(i %/% 2)) * (x1^power) + stats::runif(n, -noise_level, noise_level * 2)
    }
  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

# Function to gen a non-linear rectangular hyperbola in 4D
gen_nonlinear_hyperbola2_4d <- function(n = 500, p = 4, C = 1, nonlinear_factor = 0.5) {

  df <- matrix(0, nrow = n, ncol = p)

  # gen random points for x1 and x3 in a range avoiding zero
  df[, 1] <- runif(n, 0.1, 2)  # Avoid zero to prevent division by zero
  df[, 3] <- runif(n, 0.1, 0.8)

  # # Apply non-linear distortions for the second dimension
  df[, 2] <-  (C / x1) + nonlinear_factor * sin(x1)  # Hyperbola + sine curve distortion
  #
  # # Apply non-linear distortions for the fourth dimension
  # x4 <- (C / x3) + nonlinear_factor * cos(x3)  # Hyperbola + cosine curve distortion

  # Define additional dimensions for 4D
  #x2 <- -sin(x1 * pi) + runif(n, -0.1, 0.1)  # A sine-based curve
  df[, 4] <- cos(x1 * pi) + runif(n, -0.1, 0.1)   # A cosine-based curve

  # Extend to higher dimensions
  if (p > 4) {
    for (i in 5:d) {
      # Introduce non-linearity based on x1 and add random noise
      # You can experiment with different non-linear functions and noise levels
      power <- sample(2:5, 1) # Random power for the polynomial
      scale_factor <- stats::runif(1, 0.5, 2) # Random scaling
      noise_level <- stats::runif(1, 0, 1)

      df[, i] <- scale_factor * ((-1)^(i %/% 2)) * (x1^power) + stats::runif(n, -noise_level, noise_level * 2)
    }
  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
