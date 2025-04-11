#' Generate Small Spheres Within a Big Sphere
#'
#' This function generates a dataset representing a structure with a small and big spheres.
#'
#' @param n A numeric vector (default: c(1000, 100)) representing the sample sizes of the big and small spheres respectively.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 3) representing the number of small spheres.
#' @param r A numeric vector (default: c(15, 3)) representing the radius of the big and small spheres respectively.
#' @param loc A numeric value (default: 10 / sqrt(3) representing how far the small spheres are placed from each other.
#' @return A data containing small spheres within a big sphere.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' sphere_data <- gen_clusteredSpheres(n = 500, p = 4, r = 1)
#' head(sphere_data, 5)
gen_clusteredSpheres <- function(k = 3, p = 4, n = c(1000, 100), r = c(15, 3),
                                 loc = 10 / sqrt(3)) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly two values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  if (any(r < 0)) {
    cli::cli_abort("Values in r should be positive.")
  }

  n_big <- n[1]
  n_small <- n[2]

  r_big <- r[1]
  r_small <- r[2]

  d_dim_sphere <- gen_unifSphere(n_small, p, r_small)
  small_spheres <- lapply(seq_len(k), function(i) {
    center <- rnorm(p, sd = loc)
    sweep(d_dim_sphere, 2, center, "+")
  })

  big_sphere <- gen_unifSphere(n_big, p, r_big)

  small_labeled <- lapply(seq_along(small_spheres), function(i) {
    cbind(small_spheres[[i]], cluster = paste0("small_", i))
  })

  big_labeled <- cbind(big_sphere, cluster = "big")

  df <- dplyr::bind_rows(c(small_labeled, list(big_labeled))) |>
    tibble::as_tibble()
  names(df) <- append(paste0("x", 1:p), "cluster")

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate p-D Triangular Pyramid With Triangular Pyramid shaped holes
#'
#' This function generates p-D triangular pyramid with triangular pyramid shaped holes.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a triangular pyramid with triangular pyramid shaped holes.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' triangular_data <- gen_pyrHoles(n = 500, p = 3)
gen_pyrHoles <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  trace_point <- stats::runif(p)
  corner_points <- geozoo::simplex(p=p)$points

  data_list <- lapply(1:p, function(i) rep(0, n))
  names(data_list) <- paste0("x", 1:p)

  df <- tibble::tibble(!!!data_list)

  for (i in 1:n) {
    trace_point <- (corner_points[sample((p + 1), 1), ] + trace_point) / 2
    df[i, ] <- as.list(trace_point)
  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
