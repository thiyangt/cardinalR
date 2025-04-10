#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_01(n = c(700, 300, 500))
gen_three_clust_01 <- function(n = c(700, 300, 500)) {

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  curvilinear_cluster <- gen_curv_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
  mutate(cluster = "cluster1")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[2],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
  mutate(cluster = "cluster2")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[3],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
  mutate(cluster = "cluster3")


  df <- bind_rows(curvilinear_cluster,
                  elliptical_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_02(n = c(700, 300, 500))
gen_three_clust_02 <- function(n = c(700, 300, 500)) {

  nonlinear_cluster <- gen_s_curve_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  cube_cluster <- gen_cube_4d(
    n = n[2],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[3],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(nonlinear_cluster,
                  cube_cluster,
                  rect_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_03(n = c(700, 300, 500))
gen_three_clust_03 <- function(n = c(700, 300, 500)) {

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n[1],
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[2],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[3],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(nonlinear_cluster,
                  hemisphere_cluster,
                  tri_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_04(n = c(700, 300, 500))
gen_three_clust_04 <- function(n = c(700, 300, 500)) {

  curvilinear_cluster <- gen_curv2_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[2],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[3],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(curvilinear_cluster,
                  gau_cluster,
                  hex_pyr_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_05(n = c(700, 300, 500))
gen_three_clust_05 <- function(n = c(700, 300, 500)) {

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[2],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[3],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(hyperbola_cluster,
                  elliptical_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_06(n = c(700, 300, 500))
gen_three_clust_06 <- function(n = c(700, 300, 500)) {

  curvilinear_cluster <- gen_crescent_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  cube_cluster <- gen_cube_4d(
    n = n[2],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[3],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(curvilinear_cluster,
                  cube_cluster,
                  rect_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_07(n = c(700, 300, 500))
gen_three_clust_07 <- function(n = c(700, 300, 500)) {

  hyperbola_cluster <- gen_nonlinear_hyperbola2_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[2],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[3],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(hyperbola_cluster,
                  hemisphere_cluster,
                  tri_corn_cluster)

  df <- randomize_rows(df)

  df

}


#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_08(n = c(700, 300, 500))
gen_three_clust_08 <- function(n = c(700, 300, 500)) {

  spiral_cluster <- gen_conic_spiral_4d(
    n = n[1],
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[2],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[3],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(spiral_cluster,
                  gau_cluster,
                  hex_pyr_cluster)

  df <- randomize_rows(df)

  df

}


#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_09(n = c(700, 300, 500))
gen_three_clust_09 <- function(n = c(700, 300, 500)) {

  helical_cluster <- gen_helical_hyper_spiral_4d(
    n = n[1],
    a = 0.1,
    b = 0.1,
    k = 2,
    spiral_radius = 1,
    scale_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  cube_cluster <- gen_cube_4d(
    n = n[2],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[3],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(helical_cluster,
                  cube_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}


#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_10(n = c(700, 300, 500))
gen_three_clust_10 <- function(n = c(700, 300, 500)) {

  spherical_spiral_cluster <- gen_spherical_spiral_4d(
    n = n[1],
    radius = 1,
    spiral_turns = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[2],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[3],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(spherical_spiral_cluster,
                  gau_cluster,
                  tri_corn_cluster)

  df <- randomize_rows(df)

  df

}


#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_11(n = c(700, 300, 500))
gen_three_clust_11 <- function(n = c(700, 300, 500)) {

  curvilinear_cluster <- gen_curv_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[2],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[3],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(curvilinear_cluster,
                  elliptical_cluster,
                  rect_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_12(n = c(700, 300, 500))
gen_three_clust_12 <- function(n = c(700, 300, 500)) {

  nonlinear_cluster <- gen_s_curve_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[2],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[3],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(nonlinear_cluster,
                  hemisphere_cluster,
                  hex_pyr_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_13(n = c(700, 300, 500))
gen_three_clust_13 <- function(n = c(700, 300, 500)) {

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n[1],
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  cube_cluster <- gen_cube_4d(
    n = n[2],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[3],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(nonlinear_cluster,
                  cube_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_14(n = c(700, 300, 500))
gen_three_clust_14 <- function(n = c(700, 300, 500)) {

  curvilinear_cluster <- gen_curv2_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[2],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[3],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(curvilinear_cluster,
                  gau_cluster,
                  tri_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_15(n = c(700, 300, 500))
gen_three_clust_15 <- function(n = c(700, 300, 500)) {

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[2],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[3],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(hyperbola_cluster,
                  elliptical_cluster,
                  rect_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_16(n = c(700, 300, 500))
gen_three_clust_16 <- function(n = c(700, 300, 500)) {

  curvilinear_cluster <- gen_crescent_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[2],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[3],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(curvilinear_cluster,
                  hemisphere_cluster,
                  hex_pyr_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_17(n = c(700, 300, 500))
gen_three_clust_17 <- function(n = c(700, 300, 500)) {

  hyperbola_cluster <- gen_nonlinear_hyperbola2_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  cube_cluster <- gen_cube_4d(
    n = n[2],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[3],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(hyperbola_cluster,
                  cube_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_18(n = c(700, 300, 500))
gen_three_clust_18 <- function(n = c(700, 300, 500)) {

  spiral_cluster <- gen_conic_spiral_4d(
    n = n[1],
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[2],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[3],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(spiral_cluster,
                  gau_cluster,
                  tri_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_19(n = c(700, 300, 500))
gen_three_clust_19 <- function(n = c(700, 300, 500)) {

  helical_cluster <- gen_helical_hyper_spiral_4d(
    n = n[1],
    a = 0.1,
    b = 0.1,
    k = 2,
    spiral_radius = 1,
    scale_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[2],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[3],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(helical_cluster,
                  hemisphere_cluster,
                  hex_pyr_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_20(n = c(700, 300, 500))
gen_three_clust_20 <- function(n = c(700, 300, 500)) {

  spherical_spiral_cluster <- gen_spherical_spiral_4d(
    n = n[1],
    radius = 1,
    spiral_turns = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[2],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[3],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(spherical_spiral_cluster,
                  elliptical_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_21(n = c(700, 300, 500))
gen_three_clust_21 <- function(n = c(700, 300, 500)) {

  curvilinear_cluster <- gen_curv_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[2],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[3],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(curvilinear_cluster,
                  gau_cluster,
                  rect_corn_cluster)

  df <- randomize_rows(df)

  df

}


#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_22(n = c(700, 300, 500))
gen_three_clust_22 <- function(n = c(700, 300, 500)) {

  nonlinear_cluster <- gen_s_curve_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  cube_cluster <- gen_cube_4d(
    n = n[2],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[3],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(nonlinear_cluster,
                  cube_cluster,
                  tri_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_23(n = c(700, 300, 500))
gen_three_clust_23 <- function(n = c(700, 300, 500)) {

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n[1],
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[2],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[3],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(nonlinear_cluster,
                  hemisphere_cluster,
                  hex_pyr_cluster)

  df <- randomize_rows(df)

  df

}


#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_24(n = c(700, 300, 500))
gen_three_clust_24 <- function(n = c(700, 300, 500)) {

  curvilinear_cluster <- gen_curv2_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[2],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[3],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(curvilinear_cluster,
                  elliptical_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}


#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_25(n = c(700, 300, 500))
gen_three_clust_25 <- function(n = c(700, 300, 500)) {

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[2],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[3],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(hyperbola_cluster,
                  gau_cluster,
                  rect_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_26(n = c(700, 300, 500))
gen_three_clust_26 <- function(n = c(700, 300, 500)) {

  curvilinear_cluster <- gen_crescent_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  cube_cluster <- gen_cube_4d(
    n = n[2],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[3],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(curvilinear_cluster,
                  cube_cluster,
                  tri_corn_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_27(n = c(700, 300, 500))
gen_three_clust_27 <- function(n = c(700, 300, 500)) {

  hyperbola_cluster <- gen_nonlinear_hyperbola2_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[2],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[3],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(hyperbola_cluster,
                  hemisphere_cluster,
                  hex_pyr_cluster)

  df <- randomize_rows(df)

  df

}

#' Generate Three Different Shaped Clusters
#'
#' This function generates three different shaped clusters.
#'
#' @param n A numeric vector (default: c(700, 300, 500)) representing the sample sizes.
#' @return A data containing the three different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gen_three_clust_28(n = c(700, 300, 500))
gen_three_clust_28 <- function(n = c(700, 300, 500)) {

  spiral_cluster <- gen_conic_spiral_4d(
    n = n[1],
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[2],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[3],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  df <- bind_rows(spiral_cluster,
                  elliptical_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}
