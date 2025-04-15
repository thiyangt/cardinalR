#' Generate Curvy
#'
#' This function generates a dataset representing a structure with a curvy pattern.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a curvy structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' curvy_data <- gen_curve(n = 500, p = 4)
gen_curve <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- matrix(0, nrow = n, ncol = p)
  # gen the core curvilinear pattern in 2D
  df[, 1] <- stats::runif(n, 0, 2)
  poly_basis <- stats::poly(df[, 1], degree = 2, raw = TRUE)
  df[, 2] <- -poly_basis[, 2] + stats::runif(n, 0, 0.5)

  if (p > 2){

    if(p==3) {
      # Define additional dimensions for 4D
      df[, 3] <- -sin(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)  # A sine-based curve

    } else if (p == 4) {
      df[, 3] <- -sin(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)  # A sine-based curve
      df[, 4] <- cos(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)   # A cosine-based curve

    } else {

      df[, 3] <- -sin(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)  # A sine-based curve
      df[, 4] <- cos(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)   # A cosine-based curve

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

  df <- matrix(0, nrow = n, ncol = p)
  # gen the core curvilinear pattern in 2D
  df[, 1] <- cos(theta)
  df[, 2] <- sin(theta)

  if (p > 2){

    if(p==3) {
      # Define additional dimensions for 4D
      df[, 3] <- theta + stats::rnorm(n, 0, 0.5)  # Simply map theta to the third dimension

    } else if (p == 4) {
      df[, 3] <- theta + stats::rnorm(n, 0, 0.5)  # Simply map theta to the third dimension
      df[, 4] <- 2 * theta + stats::rnorm(n, 0, 0.5)  # Linear function for the fourth dimension

    } else {

      df[, 3] <- theta + stats::rnorm(n, 0, 0.5)  # Simply map theta to the third dimension
      df[, 4] <- 2 * theta + stats::rnorm(n, 0, 0.5)  # Linear function for the fourth dimension

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

#' Generate Curvy Cylinder
#'
#' This function generates a dataset representing a structure with a curvy cylinder.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param r A numeric value (default: 1) representing the radius of the cylinder.
#' @param h A numeric value (default: 10) representing the height of the cylinder.
#' @param a A numeric value (default: 1) representing the strength of the cylinder in 4th dimension.
#' @return A data containing a curvy cylinder.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_curvyCylinder(n = 500, p = 4, r = 1, h = 10, a = 1)
gen_curvyCylinder <- function(n = 500, p = 4, r = 1, h = 10, a = 1) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  # Step 1: gen cylindrical coordinates in 2D (x1, x2)
  theta <- stats::runif(n, 0, 3 * pi)  # Random angle for the circular base
  df <- matrix(0, nrow = n, ncol = p)

  df[, 1] <- r * cos(theta)            # x1 coordinate (circular)
  df[, 2] <- r * sin(theta)            # x2 coordinate (circular)

  if (p > 2){

    if(p==3) {
      # Define additional dimensions for 4D
      df[, 3] <- stats::runif(n, 0, h)     # Height along the cylinder

    } else if (p == 4) {
      df[, 3] <- stats::runif(n, 0, h)     # Height along the cylinder
      df[, 4] <- a * sin(df[, 3])       # Curvy pattern in the 4th dimension

    } else {

      df[, 3] <- stats::runif(n, 0, h)     # Height along the cylinder
      df[, 4] <- a * sin(df[, 3])       # Curvy pattern in the 4th dimension

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

#' Generate Spherical Spiral
#'
#' This function generates a dataset representing a structure with a spherical spiral.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param r A numeric value (default: 1) representing the radius of the circle.
#' @param spins A numeric value (default: 1) representing the number of loops of the spiral.
#' @return A data containing a spherical spiral.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_sphericalSpiral(n = 500, p = 4, r = 1, spins = 1)
gen_sphericalSpiral <- function(n = 500, p = 4, r = 1, spins = 1) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  # gen angles (theta, phi) for the spherical coordinates
  theta <- seq(0, spins * 2 * pi, length.out = n)  # Controls the number of spiral turns
  phi <- seq(0, pi, length.out = n)                       # Controls movement along the latitude

  df <- matrix(0, nrow = n, ncol = p)

  # Spherical to Cartesian coordinates for 4D
  df[, 1] <- r * sin(phi) * cos(theta)
  df[, 2] <- r * sin(phi) * sin(theta)
  df[, 3] <- r * cos(phi) + stats::runif(n, -0.5, 0.5)

  if(p > 3) {
    if(p == 4) {

      df[, 4] <- theta / max(theta) * r  # Spiral along the 4th dimension

    } else {

      for (i in 5:p) {
        # Introduce non-linearity based on x1 and add random noise
        # You can experiment with different non-linear functions and noise levels
        power <- sample(2:5, 1) # Random power for the polynomial
        scale_factor <- stats::runif(1, 0.5, 2) # Random scaling
        noise_level <- stats::runif(1, 0, 0.05)

        df[, i] <- scale_factor * ((-1)^(i %/% 2)) * (df[, 1]^power) + stats::runif(n, -noise_level, noise_level * 2)
      }

    }
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
#' @param a A numeric value (default: 0.05) representing the scale factor along x2.
#' @param b A numeric value (default: 0.1) representing the scale factor along x3.
#' @param c A numeric value (default: 1) representing the scale factor of sine along x3.
#' @param r A numeric value (default: 0.5) representing the radius of the spiral.
#' @return A data containing a helical hyper spiral.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_helicalHyperspiral(n = 500, p = 4, a = 0.05, b = 0.1, c = 1, r = 0.5)
gen_helicalHyperspiral <- function(n = 500, p = 4, a = 0.05, b = 0.1, c = 1, r = 0.5) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  # gen angles for the spiral (theta)
  theta <- seq(0, 5/4 * pi, length.out = n)

  df <- matrix(0, nrow = n, ncol = p)

  # Helical spiral coordinates
  df[, 1] <- r * cos(theta)  # x1 is a circular pattern
  df[, 2] <- r * sin(theta)  # x2 is a circular pattern
  df[, 3] <- a * theta + stats::runif(n, -0.5, 0.5) # x3 moves linearly with theta (like a helix)

  # Extend to higher dimensions
  if (p > 3) {
    if (p == 4) {
      df[, 4] <- b * sin(c * theta)          # x4 oscillates with sin(k * theta)

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

#' Generate Conical Spiral
#'
#' This function generates a dataset representing a conical spiral structure.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param r A numeric value (default: 1) representing the radius of the circle.
#' @param spins A numeric value (default: 1) representing the number of loops of the spiral.
#' @return A data containing a conical spiral structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_conicSpiral(n = 500, p = 4, r = 1, spins = 1)
gen_conicSpiral <- function(n = 500, p = 4, spins = 1, cone_height = 2, cone_radius = 1) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  # gen theta values to represent the angle of the spiral in the xy-plane
  theta <- seq(0, 2 * pi * spins, length.out = n)
  df <- matrix(0, nrow = n, ncol = p)

  # Spiral in the first two dimensions (x1, x2) - Archimedean spiral
  r <- cone_radius * theta  # r increases linearly with theta
  df[, 1] <- r * cos(theta)
  df[, 2] <- r * sin(theta)

  # Conical shape in the third dimension (x3) - linear increase with height
  df[, 3] <- cone_height * theta / max(theta) + stats::runif(n, -0.1, 0.6) # Scaling height to range from 0 to cone_height

  # Extend to higher dimensions
  if (p > 3) {
    if (p == 4) {
      # Spiral in the fourth dimension (x4) - a helical shape based on the cone
      df[, 4] <- cone_radius * sin(2 * theta) + stats::runif(n, -0.1, 0.6) # Helical movement

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
