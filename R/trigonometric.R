#' Generate Crescent
#'
#' This function generates a dataset representing a structure with a Crescent pattern.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a Crescent structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_crescent(n = 500, p = 4)
gen_crescent <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  # Step 1: gen angles for a semi-circle
  theta <- seq(pi / 6, 12 * pi / 6, length.out = n)  # evenly spaced angles for crescent

  df <- matrix(0, nrow = n, ncol = 2)
  # gen the core curvilinear pattern in 2D
  df[, 1] <- cos(theta)
  df[, 2] <- sin(theta)

  if (p > 2) {

    noise_df <- gen_wavydims1(n = n, p = (p-2), theta = theta) |>
      as.matrix()
    colnames(noise_df) <- paste0("x", 3:p)

    df <- cbind(df, noise_df)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}


#' Generate Curvy Cylinder
#'
#' This function generates a dataset representing a structure with a curvy cylinder.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param h A numeric value (default: 10) representing the height of the cylinder.
#' @return A data containing a curvy cylinder.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_curvyCylinder(n = 500, p = 4, h = 10)
gen_curvyCylinder <- function(n = 500, p = 4, h = 10) {

  if (p < 4) {
    cli::cli_abort("p should be greater than 4.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  # Step 1: gen cylindrical coordinates in 2D (x1, x2)
  theta <- stats::runif(n, 0, 3 * pi)  # Random angle for the circular base
  df <- matrix(0, nrow = n, ncol = 4)

  df[, 1] <- cos(theta)            # x1 coordinate (circular)
  df[, 2] <- sin(theta)            # x2 coordinate (circular)
  df[, 3] <- stats::runif(n, 0, h)     # Height along the cylinder
  df[, 4] <- sin(df[, 3])       # Curvy pattern in the 4th dimension


  if (p > 4){

    noise_df <- gen_noisedims(n = n, p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4)) |>
      as.matrix()
    colnames(noise_df) <- paste0("x", 5:p)

    df <- cbind(df, noise_df)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate Spherical Spiral
#'
#' This function generates a dataset representing a structure with a spherical spiral.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param spins A numeric value (default: 1) representing the number of loops of the spiral.
#' @return A data containing a spherical spiral.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_sphericalSpiral(n = 500, p = 4, spins = 1)
gen_sphericalSpiral <- function(n = 500, p = 4, spins = 1) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  # gen angles (theta, phi) for the spherical coordinates
  theta <- seq(0, spins * 2 * pi, length.out = n)  # Controls the number of spiral turns
  phi <- seq(0, pi, length.out = n)                       # Controls movement along the latitude

  df <- matrix(0, nrow = n, ncol = 4)

  # Spherical to Cartesian coordinates for 4D
  df[, 1] <- sin(phi) * cos(theta)
  df[, 2] <- sin(phi) * sin(theta)
  df[, 3] <- cos(phi) + stats::runif(n, -0.5, 0.5)
  df[, 4] <- theta / max(theta)  # Spiral along the 4th dimension


  if(p > 4) {

    noise_df <- gen_wavydims2(n = n, p = (p-4), x1_vec = df[, 1]) |>
      as.matrix()
    colnames(noise_df) <- paste0("x", 5:p)

    df <- cbind(df, noise_df)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}


#' Generate Helical Hyper Spiral
#'
#' This function generates a dataset representing a structure with a helical hyper spiral.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a helical hyper spiral.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_helicalHyperspiral(n = 500, p = 4)
gen_helicalHyperspiral <- function(n = 500, p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  # gen angles for the spiral (theta)
  theta <- seq(0, 5/4 * pi, length.out = n)

  df <- matrix(0, nrow = n, ncol = 4)

  # Helical spiral coordinates
  df[, 1] <- cos(theta)  # x1 is a circular pattern
  df[, 2] <- sin(theta)  # x2 is a circular pattern
  df[, 3] <- 0.05 * theta + stats::runif(n, -0.5, 0.5) # x3 moves linearly with theta (like a helix)
  df[, 4] <- 0.1 * sin(theta)          # x4 oscillates with sin(k * theta)


  # Extend to higher dimensions
  if (p > 4) {

    noise_df <- gen_noisedims(n = n, p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4)) |>
      as.matrix()
    colnames(noise_df) <- paste0("x", 5:p)

    df <- cbind(df, noise_df)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Conical Spiral
#'
#' This function generates a dataset representing a conical spiral structure.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param spins A numeric value (default: 1) representing the number of loops of the spiral.
#' @return A data containing a conical spiral structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_conicSpiral(n = 500, p = 4, spins = 1)
gen_conicSpiral <- function(n = 500, p = 4, spins = 1) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  # gen theta values to represent the angle of the spiral in the xy-plane
  theta <- seq(0, 2 * pi * spins, length.out = n)
  df <- matrix(0, nrow = n, ncol = 4)

  # Spiral in the first two dimensions (x1, x2) - Archimedean spiral
  df[, 1] <- theta * cos(theta)
  df[, 2] <- theta * sin(theta)

  # Conical shape in the third dimension (x3) - linear increase with height
  df[, 3] <- 2 * theta / max(theta) + stats::runif(n, -0.1, 0.6) # Scaling height to range from 0 to cone_height

  # Spiral in the fourth dimension (x4) - a helical shape based on the cone
  df[, 4] <- theta * sin(2 * theta) + stats::runif(n, -0.1, 0.6) # Helical movement

  # Extend to higher dimensions
  if (p > 4) {

    noise_df <- gen_noisedims(n = n, p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4)) |>
      as.matrix()
    colnames(noise_df) <- paste0("x", 5:p)

    df <- cbind(df, noise_df)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Nonlinear Hyperbola
#'
#' This function generates a dataset representing a nonlinear hyperbola structure.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param hc A numeric value (default: 1) representing the hyperbolic component which define the steepness and vertical scaling of the hyperbola. Larger values of this make the curve more pronounced (sharper dips/rises near 0), while smaller values make it flatter.
#' @param non_fac A numeric value (default: 1) representing the nonlinear factor which describes the strength of this sinusoidal effect. When this is 0, the curve is purely hyperbolic; as it increases, the wave-like fluctuations become more prominent.
#' @return A data containing a nonlinear hyperbola structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_nonlinearHyperbola(n = 500, p = 4, hc = 1, non_fac = 0.5)
gen_nonlinearHyperbola <- function(n = 500, p = 4, hc = 1, non_fac = 0.5) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- matrix(0, nrow = n, ncol = p)

  # gen random points for x1 and x3 in a range avoiding zero
  df[, 1] <- stats::runif(n, 0.1, 2)  # Avoid zero to prevent division by zero
  df[, 3] <- stats::runif(n, 0.1, 0.8)

  # # Apply non-linear distortions for the second dimension
  df[, 2] <-  (hc /  df[, 1]) + non_fac * sin( df[, 1])  # Hyperbola + sine curve distortion

  # Extend to higher dimensions
  if (p > 3) {
    if(p == 4) {
      # Define additional dimensions for 4D
      df[, 4] <- cos(df[, 1] * pi) + stats::runif(n, -0.1, 0.1)   # A cosine-based curve

    } else {

      noise_df <- gen_noisedims(n = n, p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4)) |>
        as.matrix()
      colnames(noise_df) <- paste0("x", 5:p)

      df <- cbind(df, noise_df)

    }
  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

