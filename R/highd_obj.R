#' Generate overlapped tow conic spiral shapes
#'
#' This function generates overlapped tow conic spiral shapes.
#'
#' @param n A numeric vector (default: c(500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#'
#' @return A data containing the two overlapped conic spiral shapes.
#'
#' @examples
#' set.seed(20240412)
#' gen_overlap_3d_two_conic_spiral(n = c(500, 300), p = 3)
#'
#' @export
gen_overlap_3d_two_conic_spiral <- function(n = c(500, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df1 <- tibble::as_tibble(geozoo::conic.spiral(n = n[1])$points, .name_repair = "unique") |>
    set_names(paste0("x", 1:3))
  df2 <- tibble::as_tibble(geozoo::conic.spiral(n = n[2])$points[,c(3, 1, 2)], .name_repair = "unique") |>
    set_names(paste0("x", 1:3))

  df <- dplyr::bind_rows(df1, df2)

  if (p > 3) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 3,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 4:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate intersected tow conic spiral shapes
#'
#' This function generates intersected tow conic spiral shapes.
#'
#' @param n A numeric vector (default: c(500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#'
#' @return A data containing the two intersected conic spiral shapes.
#'
#' @examples
#' set.seed(20240412)
#' gen_intersect_3d_two_conic_spiral(n = c(500, 300), p = 3)
#'
#' @export
gen_intersect_3d_two_conic_spiral <- function(n = c(500, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df1 <- tibble::as_tibble(geozoo::conic.spiral(n = n[1])$points, .name_repair = "unique") |>
    set_names(paste0("x", 1:3))
  df2 <- tibble::as_tibble(geozoo::conic.spiral(n = n[2])$points[,c(2, 1, 3)], .name_repair = "unique") |>
    set_names(paste0("x", 1:3))

  df <- dplyr::bind_rows(df1, df2)

  if (p > 3) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 3,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 4:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate points on a Dini's surface.
#'
#' This function generates points on a Dini's surface.
#'
#' @param a Outer radius of the surface.
#' @param b Space between loops.
#'
#' @return A matrix containing the generated points on the surface.
#'
#' @examples
#' set.seed(20240412)
#' dini_surface_3d_row(a = 1, b = 1)
#'
#' @export
dini_surface_3d_row <- function(a = 1, b = 1) {
  u <- stats::runif(1, min = 0, max = 4 * pi)
  v <- stats::runif(1, min = 0.0000000001, max = 2)
  x <- a * cos(u) * sin(v)
  y <- a * sin(u) * sin(v)
  z <- a * (cos(v) + log(tan(v / 2))) + (b * u)
  return(cbind(x, y, z))
}

#' Generate points sampled from the Dini surface with optional noise.
#'
#' This function generates points sampled from the Dini surface along with optional noise.
#'
#' @param n Total number of data points to generate.
#' @param num_noise Number of additional noise dimensions to add to the data.
#' @param min_n Minimum value for the noise added to the data.
#' @param max_n Maximum value for the noise added to the data.
#'
#' @return A matrix containing the generated data points with or without added noise.
#' @importFrom purrr reduce
#'
#' @examples
#' set.seed(20240412)
#' dini_surface_3d(n = 100, num_noise = 2, min_n = -0.05, max_n = 0.05)
#'
#' @export
dini_surface_3d <- function(n, num_noise, min_n, max_n) {
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

  df_list <- lapply(1:n, function(i) dini_surface_3d_row(a = 1, b = 1))
  df <- purrr::reduce(df_list, rbind)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}

#' Generate points on a Roman surface in 3D space.
#'
#' This function generates points on a Roman surface in 3D space.
#'
#' @param a Maximum radius of the object.
#'
#' @return A matrix containing the generated points on the Roman surface in 3D space.
#'
#' @examples
#' set.seed(20240412)
#' roman_surface_3d_row(a = 1)
#'
#' @export
roman_surface_3d_row <- function(a = 1) {
  u <- stats::runif(1, min = 0, max = pi)
  v <- stats::runif(1, min = 0, max = pi)
  x <- a^2 * cos(v) * cos(v) * sin(2 * u) / 2
  y <- a^2 * sin(u) * sin(2 * v) / 2
  z <- a^2 * cos(u) * sin(2 * v) / 2
  return(cbind(x, y, z))
}

#' Generate data points on a Roman surface with optional noise.
#'
#' This function generates data points on a Roman surface with optional noise.
#'
#' @param n Total number of data points to generate.
#' @param num_noise Number of additional noise dimensions to add to the data.
#' @param min_n Minimum value for the noise added to the data.
#' @param max_n Maximum value for the noise added to the data.
#'
#' @return A matrix containing the generated data points with or without added noise.
#' @importFrom purrr reduce
#'
#' @examples
#' set.seed(20240412)
#' roman_surface_3d(n = 100, num_noise = 2, min_n = -0.05, max_n = 0.05)
#'
#' @export
roman_surface_3d <- function(n, num_noise, min_n, max_n) {
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

  df_list <- lapply(1:n, function(i) roman_surface_3d_row(a = 1))
  df <- purrr::reduce(df_list, rbind)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}

#' Generate a spiral dataset with optional noise.
#'
#' This function generates a dataset arranged in a spiral pattern with optional noise.
#'
#' @param n Total number of data points to generate.
#' @param num_dims Number of effective dimensions for each data point.
#' @param num_noise Number of additional noise dimensions to add to the data.
#' @param min_n Minimum value for the noise added to the data.
#' @param max_n Maximum value for the noise added to the data.
#'
#' @return A matrix containing the generated data points with or without added noise.
#' @importFrom purrr reduce
#'
#' @examples
#' set.seed(20240412)
#' spiral_3d(n = 100, num_dims = 10, num_noise = 2, min_n = -0.05, max_n = 0.05)
#'
#' @export
spiral_3d <- function(n, num_dims, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (num_dims < 0) {
    stop("Number of effective dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  if (missing(num_dims)) {
    stop("Missing num_dims.")
  }

  u <- array(stats::runif(n = (n * 1), min = 0, max = 5), dim = c(n, 1))
  df <- array(cos(pi * u), dim = c(n, num_dims))
  y <- u * sin(pi * u)
  if (num_dims > 1) {
    for (i in 1:(num_dims - 1)) {
      df[, i] <- y * df[, i, drop = FALSE]^i
    }
  }
  df[, num_dims] <- u * df[, num_dims, drop = FALSE]

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}

#' Generate overlapped tow torus shapes
#'
#' This function generates overlapped tow torus shapes.
#'
#' @param n A numeric vector (default: c(500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#'
#' @return A data containing the two overlapped torus shapes.
#'
#' @examples
#' set.seed(20240412)
#' gen_overlap_3d_two_torus(n = c(500, 300), p = 3)
#'
#' @export
gen_overlap_3d_two_torus <- function(n = c(500, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df1 <- tibble::as_tibble(geozoo::torus(p = 3, n = n[1])$points, .name_repair = "unique") |>
    set_names(paste0("x", 1:3))
  df2 <- tibble::as_tibble(geozoo::torus(p = 3, n = n[2])$points[,c(3, 1, 2)], .name_repair = "unique") |>
  set_names(paste0("x", 1:3))

  df <- dplyr::bind_rows(df1, df2)

  if (p > 3) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 3,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 4:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate a 3D cube with optional noise.
#'
#' This function generates a 3D cube along with optional noise.
#'
#' @param num_dims Number of effective dimensions (default is 3 for a 3D cube).
#' @param num_noise Number of additional noise dimensions to add to the data.
#' @param min_n Minimum value for the noise added to the data.
#' @param max_n Maximum value for the noise added to the data.
#'
#' @return A list containing the generated data matrix and the sample size.
#' @importFrom purrr reduce
#'
#' @examples
#' set.seed(20240412)
#' cube_3d(num_dims = 3, num_noise = 2, min_n = -0.01, max_n = 0.01)
#'
#' @export
cube_3d <- function(num_dims, num_noise, min_n, max_n) {
  if (num_dims <= 0) {
    stop("Number of effective dimensions should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  if (missing(num_dims)) {
    stop("Missing num_dims.")
  }

  df1 <- expand.grid(replicate(num_dims, (0:11) / 11, simplify = FALSE))
  df2 <- expand.grid(replicate(num_dims, c(0, 1), simplify = FALSE))

  df <- unique(rbind(as.matrix(df1), as.matrix(df2)))

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)
  }

  return(list(df = df, n = NROW(df)))
}
