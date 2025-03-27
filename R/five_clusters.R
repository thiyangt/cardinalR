###################Generate five clusters

## Data structure 1
five_clust_01 <- function(n = 1500, pentagon_vertices) {

  curvilinear_cluster <- gen_curv_4d(
    n = n * 5/15,
    offset = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x1",
             "x3" = "x2",
             "x1" = "x3",
             "x2" = "x4")) |>
    dplyr::select(x1, x2, x3, x4) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x2",
             "x3" = "x1",
             "x1" = "x3",
             "x2" = "x4")) |>
    dplyr::select(x1, x2, x3, x4) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 3/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")


  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 2/15,
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  cube_cluster <- gen_cube_4d(
    n = n * 1/15,
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster5")


  df <- bind_rows(curvilinear_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  hemisphere_cluster,
                  cube_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 2
five_clust_02 <- function(n = 1500, pentagon_vertices) {

  nonlinear_cluster <- gen_s_curve_4d(
    n = n * 5/15,
    offset = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x1",
             "x3" = "x4",
             "x1" = "x2",
             "x2" = "x3")) |>
    dplyr::select(x1, x2, x3, x4) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 4/15,
    height = 3,
    base_width_x = 3,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x2",
             "x3" = "x1",
             "x1" = "x3",
             "x2" = "x4")) |>
    dplyr::select(x1, x2, x3, x4) |>
    mutate(cluster = "cluster2")


  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  helical_cluster <- gen_helical_hyper_spiral_4d(
    n = n * 2/15,
    a = 0.1,
    b = 0.1,
    k = 2,
    spiral_radius = 1,
    scale_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 1/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster5")


  df <- bind_rows(nonlinear_cluster,
                  rect_corn_cluster,
                  cube_cluster,
                  helical_cluster,
                  gau_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 3

five_clust_03 <- function(n = 1500, pentagon_vertices) {

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n * 5/15,
    radius = 2,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  )

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 7,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x2",
             "x3" = "x1",
             "x1" = "x3",
             "x2" = "x4")) |>
    dplyr::select(x1, x2, x3, x4)


  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 3/15,
    radius = 2,
    offset = c(0, 0, 0, 0)
  )

  curvilinear_cluster <- gen_crescent_4d(
    n = n * 2/15,
    offset = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x1",
             "x3" = "x2",
             "x1" = "x3",
             "x2" = "x4")) |>
    dplyr::select(x1, x2, x3, x4)

  cube_cluster <- gen_cube_4d(
    n = n * 1/15,
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  )


  df <- bind_rows(nonlinear_cluster,
                  tri_corn_cluster,
                  hemisphere_cluster,
                  curvilinear_cluster,
                  cube_cluster)

  df <- randomize_rows(df)

  df

}


## Data structure 4

five_clust_04 <- function(n = 1500, pentagon_vertices) {

  curvilinear_cluster <- gen_curv2_4d(
    n = n * 5/15,
    offset = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x1",
             "x3" = "x4",
             "x1" = "x2",
             "x2" = "x3")) |>
    dplyr::select(x1, x2, x3, x4)

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x2",
             "x3" = "x1",
             "x1" = "x3",
             "x2" = "x4")) |>
    dplyr::select(x1, x2, x3, x4)

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  )

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n * 2/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  )

  cube_cluster <- gen_cube_4d(
    n = n * 1/15,
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  )

  df <- bind_rows(curvilinear_cluster,
                  hex_pyr_cluster,
                  gau_cluster,
                  hyperbola_cluster,
                  cube_cluster)

  df <- randomize_rows(df)

  df


}


## Data structure 5

five_clust_05 <- function(n = 1500, pentagon_vertices) {

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n * 5/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x1",
             "x3" = "x4",
             "x1" = "x2",
             "x2" = "x3")) |>
    dplyr::select(x1, x2, x3, x4)

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x2",
             "x3" = "x1",
             "x1" = "x3",
             "x2" = "x4")) |>
    dplyr::select(x1, x2, x3, x4)

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 3/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  )

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 2/15,
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  )

  cube_cluster <- gen_cube_4d(
    n = n * 1/15,
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  )

  df <- bind_rows(hyperbola_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  hex_pyr_cluster,
                  cube_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 6

five_clust_06 <- function(n = 1500, pentagon_vertices) {

  curvilinear_cluster <- gen_crescent_4d(
    n = n * 5/15,
    offset = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x1",
             "x3" = "x4",
             "x1" = "x2",
             "x2" = "x3")) |>
    dplyr::select(x1, x2, x3, x4)

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 4/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  )


  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  )


  spiral_cluster <- gen_conic_spiral_4d(
    n = n * 2/15,
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x1",
             "x3" = "x4",
             "x1" = "x2",
             "x2" = "x3")) |>
    dplyr::select(x1, x2, x3, x4)


  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 1/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  )

  df <- bind_rows(curvilinear_cluster,
                  rect_corn_cluster,
                  cube_cluster,
                  spiral_cluster,
                  elliptical_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 7

five_clust_07 <- function(n = 1500, pentagon_vertices) {

  hyperbola_cluster2 <- gen_nonlinear_hyperbola2_4d(
    n = n * 5/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  )

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  )

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 3/15,
    radius = 1,
    offset = c(0, 0, 0, 0)
  )

  cube_cluster <- gen_cube_4d(
    n = n * 2/15,
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  )

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 1/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  )

  df <- bind_rows(hyperbola_cluster2,
                  tri_corn_cluster,
                  hemisphere_cluster,
                  cube_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 8

five_clust_08 <- function(n = 1500, pentagon_vertices) {

  spiral_cluster <- gen_conic_spiral_4d(
    n = n * 5/15,
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = c(0, 0, 0, 0)
  )

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  )

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  )

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 2/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  )

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 1/15,
    radius = 1,
    offset = c(0, 0, 0, 0)
  )

  df <- bind_rows(spiral_cluster,
                  hex_pyr_cluster,
                  gau_cluster,
                  rect_corn_cluster,
                  hemisphere_cluster)

  df <- randomize_rows(df)

  df

}


## Data structure 9

five_clust_09 <- function(n = 1500, pentagon_vertices) {

  helical_cluster <- gen_helical_hyper_spiral_4d(
    n = n * 5/15,
    a = 0.1,
    b = 0.1,
    k = 2,
    spiral_radius = 1,
    scale_factor = 0.5,
    offset = c(0, 0, 0, 0)
  )

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  )

  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  )

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 2/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  )

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 1/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  )

  df <- bind_rows(helical_cluster,
                  blunted_corn_cluster,
                  cube_cluster,
                  tri_corn_cluster,
                  elliptical_cluster)

  df <- randomize_rows(df)

  df


}


## Data structure 10

five_clust_10 <- function(n = 1500, pentagon_vertices) {

  spherical_spiral_cluster <- gen_spherical_spiral_4d(
    n = n * 5/15,
    radius = 1,
    spiral_turns = 1,
    offset = c(0, 0, 0, 0)
  )

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  )

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  )

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n * 2/15,
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  )

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 1/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x2",
             "x3" = "x1",
             "x1" = "x3",
             "x2" = "x4")) |>
    dplyr::select(x1, x2, x3, x4)


  df <- bind_rows(spherical_spiral_cluster,
                  tri_corn_cluster,
                  gau_cluster,
                  nonlinear_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 11

five_clust_11 <- function(n = 1500, pentagon_vertices) {

  curvilinear_cluster <- gen_curv_4d(
    n = n * 5/15,
    offset = c(0, 0, 0, 0)
  )

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 4/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  )

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 3/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  )

  nonlinear_cluster <- gen_s_curve_4d(
    n = n * 2/15,
    offset = c(0, 0, 0, 0)
  )

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 1/15,
    radius = 1,
    offset = c(0, 0, 0, 0)
  )

  df <- bind_rows(curvilinear_cluster,
                  rect_corn_cluster,
                  elliptical_cluster,
                  nonlinear_cluster,
                  hemisphere_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 12

five_clust_12 <- function(n = 1500, pentagon_vertices) {

  nonlinear_cluster <- gen_s_curve_4d(
    n = n * 5/15,
    offset = c(0, 0, 0, 0)
  )

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  )

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 3/15,
    radius = 1,
    offset = c(0, 0, 0, 0)
  )

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 2/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  )

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 1/15,
    height = 5,
    base_width = 7,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x2",
             "x3" = "x1",
             "x1" = "x3",
             "x2" = "x4")) |>
    dplyr::select(x1, x2, x3, x4)


  df <- bind_rows(nonlinear_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  gau_cluster,
                  tri_corn_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 13

five_clust_13 <- function(n = 1500, pentagon_vertices) {

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n * 5/15,
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  )

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  )

  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  )

  curvilinear_cluster <- gen_curv_4d(
    n = n * 2/15,
    offset = c(0, 0, 0, 0)
  )

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 1/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  )

  df <- bind_rows(nonlinear_cluster,
                  blunted_corn_cluster,
                  cube_cluster,
                  curvilinear_cluster,
                  elliptical_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 14

five_clust_14 <- function(n = 1500, pentagon_vertices) {

  curvilinear_cluster <- gen_curv2_4d(
    n = n * 5/15,
    offset = c(0, 0, 0, 0)
  )

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  )

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  )

  cube_cluster <- gen_cube_4d(
    n = n * 2/15,
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  )

  nonlinear_cluster <- gen_s_curve_4d(
    n = n * 1/15,
    offset = c(0, 0, 0, 0)
  )

  df <- bind_rows(curvilinear_cluster,
                  tri_corn_cluster,
                  gau_cluster,
                  cube_cluster,
                  nonlinear_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 15

five_clust_15 <- function(n = 1500, pentagon_vertices) {

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n * 5/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  )

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 4/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  )

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 3/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  )

  curvilinear_cluster <- gen_crescent_4d(
    n = n * 2/15,
    offset = c(0, 0, 0, 0)
  )

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 1/15,
    radius = 1,
    offset = c(0, 0, 0, 0)
  )


  df <- bind_rows(hyperbola_cluster,
                  rect_corn_cluster,
                  elliptical_cluster,
                  curvilinear_cluster,
                  hemisphere_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 16

five_clust_16 <- function(n = 1500, pentagon_vertices) {

  curvilinear_cluster <- gen_crescent_4d(
    n = n * 5/15,
    offset = c(0, 0, 0, 0)
  )

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  )

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 3/15,
    radius = 1,
    offset = c(0, 0, 0, 0)
  )

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 2/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  )

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 1/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x2",
             "x3" = "x1",
             "x1" = "x3",
             "x2" = "x4")) |>
    dplyr::select(x1, x2, x3, x4)

  df <- bind_rows(curvilinear_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  rect_corn_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 17

five_clust_17 <- function(n = 1500, pentagon_vertices) {

  hyperbola_cluster2 <- gen_nonlinear_hyperbola2_4d(
    n = n * 5/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  )

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  )

  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  )

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n * 2/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  )

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 1/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  )

  df <- bind_rows(hyperbola_cluster2,
                  blunted_corn_cluster,
                  cube_cluster,
                  hyperbola_cluster,
                  gau_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 18

five_clust_18 <- function(n = 1500, pentagon_vertices) {

  spiral_cluster <- gen_conic_spiral_4d(
    n = n * 5/15,
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = c(0, 0, 0, 0)
  )

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  )

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  )


  nonlinear_cluster <- gen_s_curve_4d(
    n = n * 2/15,
    offset = c(0, 0, 0, 0)
  )

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 1/15,
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    rename(c("x4" = "x2",
             "x3" = "x1",
             "x1" = "x3",
             "x2" = "x4")) |>
    dplyr::select(x1, x2, x3, x4)

  df <- bind_rows(spiral_cluster,
                  tri_corn_cluster,
                  gau_cluster,
                  nonlinear_cluster,
                  hex_pyr_cluster)

  df <- randomize_rows(df)

  df

}
