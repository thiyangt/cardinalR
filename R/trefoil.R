#' Generate 4-D Trefoil Knot Coordinates
#'
#' This function generates coordinates for a 4-D trefoil knot.
#' The number of points is determined by the length of the theta and phi sequences.
#'
#' @param theta A numeric vector of angles for the first parameter (e.g., from 0 to 2*pi).
#' @param phi A numeric vector of angles for the second parameter (e.g., from 0 to 4*pi).
#' @return A data containing 4-D trefoil knot.
#' @export
#' @examples
#' set.seed(20240412)
#' theta_angles <- seq(0, by = 0.175, length.out = 6)
#' phi_angles   <- seq(0, 4 * pi, length.out = 400)
#' trefoil4d <- gen_trefoil4d(theta = theta_angles, phi = phi_angles)
gen_trefoil4d <- function(theta, phi) {
  # Create a tibble of all combinations of theta and phi
  df <- tidyr::expand_grid(theta = theta, phi = phi) |>
    # Calculate the 4D coordinates
    dplyr::mutate(
      x1 = cos(theta) * cos(phi),
      x2 = cos(theta) * sin(phi),
      x3 = sin(theta) * cos(1.5 * phi),
      x4 = sin(theta) * sin(1.5 * phi)
    ) |>
    # Reorder columns for clarity, putting inputs first
    dplyr::select(x1, x2, x3, x4)

  return(df)
}


#' Generate 3-D Trefoil Knot Coordinates (Stereographic Projection)
#'
#' This function generates coordinates for a 3-D trefoil knot
#' by applying a stereographic projection from 4-D space.
#'
#' @param theta A numeric vector of angles for the first parameter (e.g., from 0 to 2*pi).
#' @param phi A numeric vector of angles for the second parameter (e.g., from 0 to 4*pi).
#' @return A data containing 3-D trefoil knot.
#' @export
#' @examples
#' set.seed(20240412)
#' theta_angles <- seq(0, by = 0.175, length.out = 6)
#' phi_angles   <- seq(0, 4 * pi, length.out = 400)
#' trefoil3d <- gen_trefoil3d(theta = theta_angles, phi = phi_angles)
gen_trefoil3d <- function(theta, phi) {
  # First, get the 4D coordinates using the tibble-returning function
  df <- gen_trefoil4d(theta, phi) |>
    # Filter out points where w is 1 to avoid division by zero
    dplyr::filter(x4 != 1) |>
    # Apply the stereographic projection
    dplyr::mutate(
      x1 = x1 / (1 - x4),
      x2 = x2 / (1 - x4),
      x3 = x3 / (1 - x4)
    ) |>
    # Select the 3D coordinates plus original parameters if desired
    dplyr::select(x1, x2, x3)

  return(df)
}
