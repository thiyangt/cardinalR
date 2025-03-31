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

#' Generate intersected tow dini surface shapes
#'
#' This function generates intersected tow dini surface shapes.
#'
#' @param n A numeric vector (default: c(500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#'
#' @return A data containing the two intersected dini surface shapes.
#'
#' @examples
#' set.seed(20240412)
#' gen_intersect_3d_two_dini_surface(n = c(500, 300), p = 3)
#' @export
gen_intersect_3d_two_dini_surface <- function(n = c(500, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df1 <- tibble::as_tibble(geozoo::dini.surface(n = n[1])$points, .name_repair = "unique") |>
    set_names(paste0("x", 1:3))
  df2 <- tibble::as_tibble(geozoo::dini.surface(n = n[2])$points[,c(2, 1, 3)], .name_repair = "unique") |>
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

#' Generate overlapped tow dini surface shapes
#'
#' This function generates overlapped tow dini surface shapes.
#'
#' @param n A numeric vector (default: c(500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#'
#' @return A data containing the two overlapped dini surface shapes.
#'
#' @examples
#' set.seed(20240412)
#' gen_overlap_3d_two_dini_surface(n = c(500, 300), p = 3)
#' @export
gen_overlap_3d_two_dini_surface <- function(n = c(500, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df1 <- tibble::as_tibble(geozoo::dini.surface(n = n[1])$points, .name_repair = "unique") |>
    set_names(paste0("x", 1:3))
  df2 <- tibble::as_tibble(geozoo::dini.surface(n = n[2])$points[,c(3, 2, 1)], .name_repair = "unique") |>
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

#' Generate tow roman surface shaped clusters
#'
#' This function generates tow roman surface shaped clusters
#'
#' @param n A numeric vector (default: c(500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#'
#' @return A data containing the tow roman surface shaped clusters.
#'
#' @examples
#' set.seed(20240412)
#' gen_roman_surface_3d_clusts(n = c(500, 300), p = 3)
#' @export
gen_roman_surface_3d_clusts <- function(n = c(500, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  df1 <- tibble::as_tibble(geozoo::roman.surface(n = n[1])$points + 1, .name_repair = "unique") |>
    set_names(paste0("x", 1:3))
  df2 <- tibble::as_tibble(geozoo::roman.surface(n = n[2])$points[,c(3, 1, 2)], .name_repair = "unique") |>
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
