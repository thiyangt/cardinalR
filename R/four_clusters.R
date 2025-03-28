###################Generate four clusters

## Data structure 1
four_clust_01 <- function(n = c(600, 400, 300, 200)) {

  curvilinear_cluster <- gen_curv_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[2],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[3],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[4],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  hemisphere_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 2
four_clust_02 <- function(n = c(600, 400, 300, 200)) {

  nonlinear_cluster <- gen_s_curve_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[2],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n[3],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  helical_cluster <- gen_helical_hyper_spiral_4d(
    n = n[4],
    a = 0.1,
    b = 0.1,
    k = 2,
    spiral_radius = 1,
    scale_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  rect_corn_cluster,
                  cube_cluster,
                  helical_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 3

four_clust_03 <- function(n = c(600, 400, 300, 200)) {

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n[1],
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[2],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[3],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  curvilinear_cluster <- gen_crescent_4d(
    n = n[4],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  tri_corn_cluster,
                  hemisphere_cluster,
                  curvilinear_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 4

four_clust_04 <- function(n = c(600, 400, 300, 200)) {

  curvilinear_cluster <- gen_curv2_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[2],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[3],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n[4],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  hex_pyr_cluster,
                  gau_cluster,
                  hyperbola_cluster)

  df <- randomize_rows(df)

  df


}

## Data structure 5

four_clust_05 <- function(n = c(600, 400, 300, 200)) {

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[2],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[3],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[4],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  hex_pyr_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 6

four_clust_06 <- function(n = c(600, 400, 300, 200)) {

  curvilinear_cluster <- gen_crescent_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[2],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n[3],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  spiral_cluster <- gen_conic_spiral_4d(
    n = n[4],
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  rect_corn_cluster,
                  cube_cluster,
                  spiral_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 7

four_clust_07 <- function(n = c(600, 400, 300, 200)) {

  hyperbola_cluster2 <- gen_nonlinear_hyperbola2_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[2],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[3],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  cube_cluster <- gen_cube_4d(
    n = n[4],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster2,
                  tri_corn_cluster,
                  hemisphere_cluster,
                  cube_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 8

four_clust_08 <- function(n = c(600, 400, 300, 200)) {

  spiral_cluster <- gen_conic_spiral_4d(
    n = n[1],
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[2],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[3],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[4],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(spiral_cluster,
                  hex_pyr_cluster,
                  gau_cluster,
                  rect_corn_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 9

four_clust_09 <- function(n = c(600, 400, 300, 200)) {

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

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[2],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n[3],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[4],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(helical_cluster,
                  blunted_corn_cluster,
                  cube_cluster,
                  tri_corn_cluster)

  df <- randomize_rows(df)

  df


}

## Data structure 10

four_clust_10 <- function(n = c(600, 400, 300, 200)) {

  spherical_spiral_cluster <- gen_spherical_spiral_4d(
    n = n[1],
    radius = 1,
    spiral_turns = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[2],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[3],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n[4],
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(spherical_spiral_cluster,
                  tri_corn_cluster,
                  gau_cluster,
                  nonlinear_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 11

four_clust_11 <- function(n = c(600, 400, 300, 200)) {

  curvilinear_cluster <- gen_curv_4d(
    n = n[1],
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[2],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[3],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  nonlinear_cluster <- gen_s_curve_4d(
    n = n[4],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  rect_corn_cluster,
                  elliptical_cluster,
                  nonlinear_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 12

four_clust_12 <- function(n = c(600, 400, 300, 200)) {

  nonlinear_cluster <- gen_s_curve_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[2],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[3],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[4],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  gau_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 13

four_clust_13 <- function(n = c(600, 400, 300, 200)) {

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n[1],
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[2],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n[3],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  curvilinear_cluster <- gen_curv_4d(
    n = n[4],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  blunted_corn_cluster,
                  cube_cluster,
                  curvilinear_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 14

four_clust_14 <- function(n = c(600, 400, 300, 200)) {

  curvilinear_cluster <- gen_curv2_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[2],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[3],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  cube_cluster <- gen_cube_4d(
    n = n[4],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  tri_corn_cluster,
                  gau_cluster,
                  cube_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 15

four_clust_15 <- function(n = c(600, 400, 300, 200)) {

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[2],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[3],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  curvilinear_cluster <- gen_crescent_4d(
    n = n[4],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster,
                  rect_corn_cluster,
                  elliptical_cluster,
                  curvilinear_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 16

four_clust_16 <- function(n = c(600, 400, 300, 200)) {

  curvilinear_cluster <- gen_crescent_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[2],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[3],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[4],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  rect_corn_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 17

four_clust_17 <- function(n = c(600, 400, 300, 200)) {

  hyperbola_cluster2 <- gen_nonlinear_hyperbola2_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[2],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n[3],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n[4],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster2,
                  blunted_corn_cluster,
                  cube_cluster,
                  hyperbola_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 18

four_clust_18 <- function(n = c(600, 400, 300, 200)) {

  spiral_cluster <- gen_conic_spiral_4d(
    n = n[1],
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[2],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[3],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  nonlinear_cluster <- gen_s_curve_4d(
    n = n[4],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(spiral_cluster,
                  tri_corn_cluster,
                  gau_cluster,
                  nonlinear_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 19

four_clust_19 <- function(n = c(600, 400, 300, 200)) {

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

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[2],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[3],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  curvilinear_cluster <- gen_curv_4d(
    n = n[4],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(helical_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  curvilinear_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 20

four_clust_20 <- function(n = c(600, 400, 300, 200)) {

  spherical_spiral_cluster <- gen_spherical_spiral_4d(
    n = n[1],
    radius = 1,
    spiral_turns = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[2],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[3],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  spiral_cluster <- gen_conic_spiral_4d(
    n = n[4],
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(spherical_spiral_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  spiral_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 21

four_clust_21 <- function(n = c(600, 400, 300, 200)) {

  curvilinear_cluster <- gen_curv_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[2],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[3],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n[4],
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  rect_corn_cluster,
                  gau_cluster,
                  nonlinear_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 22

four_clust_22 <- function(n = c(600, 400, 300, 200)) {

  nonlinear_cluster <- gen_s_curve_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[2],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n[3],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[4],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  tri_corn_cluster,
                  cube_cluster,
                  blunted_corn_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 23

four_clust_23 <- function(n = c(600, 400, 300, 200)) {

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n[1],
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[2],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[3],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n[4],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  hyperbola_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 24

four_clust_24 <- function(n = c(600, 400, 300, 200)) {

  curvilinear_cluster <- gen_curv2_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[2],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[3],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[4],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  hemisphere_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 25

four_clust_25 <- function(n = c(600, 400, 300, 200)) {

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n[2],
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n[3],
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[4],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster,
                  rect_corn_cluster,
                  gau_cluster,
                  elliptical_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 26

four_clust_26 <- function(n = c(600, 400, 300, 200)) {

  curvilinear_cluster <- gen_crescent_4d(
    n = n[1],
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n[2],
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n[3],
    side_length = 1,
    center_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  helical_cluster <- gen_helical_hyper_spiral_4d(
    n = n[4],
    a = 0.1,
    b = 0.1,
    k = 2,
    spiral_radius = 1,
    scale_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  tri_corn_cluster,
                  cube_cluster,
                  helical_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 27

four_clust_27 <- function(n = c(600, 400, 300, 200)) {

  hyperbola_cluster <- gen_nonlinear_hyperbola2_4d(
    n = n[1],
    C = 1,
    nonlinear_factor = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[2],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n[3],
    radius = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n[4],
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  nonlinear_cluster)

  df <- randomize_rows(df)

  df

}

## Data structure 28

four_clust_28 <- function(n = c(600, 400, 300, 200)) {

  spiral_cluster <- gen_conic_spiral_4d(
    n = n[1],
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n[2],
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n[3],
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster3")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n[4],
    height = 5,
    base_radius = 3,
    tip_point = c(0, 0, 0, 0)
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(spiral_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  hex_pyr_cluster)

  df <- randomize_rows(df)

  df

}

