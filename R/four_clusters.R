###################Generate four clusters

## Data structure 1
four_clust_01 <- function(n = 1500, triangle_vertices) {

  #cluster_size <- n/3

  curvilinear_cluster <- gen_curv_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 3/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 2/15,
    radius = 1,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  hemisphere_cluster)

  df

}

## Data structure 2
four_clust_02 <- function(n = 1500, triangle_vertices) {

  nonlinear_cluster <- gen_s_curve_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 4/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  helical_cluster <- gen_helical_hyper_spiral_4d(
    n = n * 2/15,
    a = 0.1,
    b = 0.1,
    k = 2,
    spiral_radius = 1,
    scale_factor = 0.5,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  rect_corn_cluster,
                  cube_cluster,
                  helical_cluster)

  df

}

## Data structure 3

four_clust_03 <- function(n = 1500, triangle_vertices) {

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n * 6/15,
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 3/15,
    radius = 1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  curvilinear_cluster <- gen_crescent_4d(
    n = n * 2/15,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  tri_corn_cluster,
                  hemisphere_cluster,
                  curvilinear_cluster)

  df

}

## Data structure 4

four_clust_04 <- function(n = 1500, triangle_vertices) {

  curvilinear_cluster <- gen_curv2_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 3,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n * 2/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  hex_pyr_cluster,
                  gau_cluster,
                  hyperbola_cluster)

  df


}

## Data structure 5

four_clust_05 <- function(n = 1500, triangle_vertices) {

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n * 6/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 3/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 2/15,
    height = 5,
    base_radius = 3,
    tip_point = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  hex_pyr_cluster)

  df

}

## Data structure 6

four_clust_06 <- function(n = 1500, triangle_vertices) {

  curvilinear_cluster <- gen_crescent_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 4/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  spiral_cluster <- gen_conic_spiral_4d(
    n = n * 2/15,
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  rect_corn_cluster,
                  cube_cluster,
                  spiral_cluster)

  df

}

## Data structure 7

four_clust_07 <- function(n = 1500, triangle_vertices) {

  hyperbola_cluster2 <- gen_nonlinear_hyperbola2_4d(
    n = n * 6/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 3/15,
    radius = 1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  cube_cluster <- gen_cube_4d(
    n = n * 2/15,
    side_length = 1,
    center_point = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster2,
                  tri_corn_cluster,
                  hemisphere_cluster,
                  cube_cluster)

  df

}

## Data structure 8

four_clust_08 <- function(n = 1500, triangle_vertices) {

  spiral_cluster <- gen_conic_spiral_4d(
    n = n * 6/15,
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 3,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 2/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(spiral_cluster,
                  hex_pyr_cluster,
                  gau_cluster,
                  rect_corn_cluster)

  df

}

## Data structure 9

four_clust_09 <- function(n = 1500, triangle_vertices) {

  helical_cluster <- gen_helical_hyper_spiral_4d(
    n = n * 6/15,
    a = 0.1,
    b = 0.1,
    k = 2,
    spiral_radius = 1,
    scale_factor = 0.5,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 2/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(helical_cluster,
                  blunted_corn_cluster,
                  cube_cluster,
                  tri_corn_cluster)

  df


}

## Data structure 10

four_clust_10 <- function(n = 1500, triangle_vertices) {

  spherical_spiral_cluster <- gen_spherical_spiral_4d(
    n = n * 6/15,
    radius = 1,
    spiral_turns = 1,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n * 2/15,
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(spherical_spiral_cluster,
                  tri_corn_cluster,
                  gau_cluster,
                  nonlinear_cluster)

  df

}

## Data structure 11

four_clust_11 <- function(n = 1500, triangle_vertices) {

  curvilinear_cluster <- gen_curv_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 4/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 3/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  nonlinear_cluster <- gen_s_curve_4d(
    n = n * 2/15,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  rect_corn_cluster,
                  elliptical_cluster,
                  nonlinear_cluster)

  df

}

## Data structure 12

four_clust_12 <- function(n = 1500, triangle_vertices) {

  nonlinear_cluster <- gen_s_curve_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 3,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 3/15,
    radius = 1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 2/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  gau_cluster)

  df

}

## Data structure 13

four_clust_13 <- function(n = 1500, triangle_vertices) {

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n * 6/15,
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  curvilinear_cluster <- gen_curv_4d(
    n = n * 2/15,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  blunted_corn_cluster,
                  cube_cluster,
                  curvilinear_cluster)

  df

}

## Data structure 14

four_clust_14 <- function(n = 1500, triangle_vertices) {

  curvilinear_cluster <- gen_curv2_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  cube_cluster <- gen_cube_4d(
    n = n * 2/15,
    side_length = 1,
    center_point = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  tri_corn_cluster,
                  gau_cluster,
                  cube_cluster)

  df

}

## Data structure 15

four_clust_15 <- function(n = 1500, triangle_vertices) {

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n * 6/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 4/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 3/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  curvilinear_cluster <- gen_crescent_4d(
    n = n * 2/15,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster,
                  rect_corn_cluster,
                  elliptical_cluster,
                  curvilinear_cluster)

  df

}

## Data structure 16

four_clust_16 <- function(n = 1500, triangle_vertices) {

  curvilinear_cluster <- gen_crescent_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 3,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 3/15,
    radius = 1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 2/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  rect_corn_cluster)

  df

}

## Data structure 17

four_clust_17 <- function(n = 1500, triangle_vertices) {

  hyperbola_cluster2 <- gen_nonlinear_hyperbola2_4d(
    n = n * 6/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n * 2/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster2,
                  blunted_corn_cluster,
                  cube_cluster,
                  hyperbola_cluster)

  df

}

## Data structure 18

four_clust_18 <- function(n = 1500, triangle_vertices) {

  spiral_cluster <- gen_conic_spiral_4d(
    n = n * 6/15,
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  nonlinear_cluster <- gen_s_curve_4d(
    n = n * 2/15,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(spiral_cluster,
                  tri_corn_cluster,
                  gau_cluster,
                  nonlinear_cluster)

  df

}

## Data structure 19

four_clust_19 <- function(n = 1500, triangle_vertices) {

  helical_cluster <- gen_helical_hyper_spiral_4d(
    n = n * 6/15,
    a = 0.1,
    b = 0.1,
    k = 2,
    spiral_radius = 1,
    scale_factor = 0.5,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 3,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 3/15,
    radius = 1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  curvilinear_cluster <- gen_curv_4d(
    n = n * 2/15,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(helical_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  curvilinear_cluster)

  df

}

## Data structure 20

four_clust_20 <- function(n = 1500, triangle_vertices) {

  spherical_spiral_cluster <- gen_spherical_spiral_4d(
    n = n * 6/15,
    radius = 1,
    spiral_turns = 1,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 3/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  spiral_cluster <- gen_conic_spiral_4d(
    n = n * 2/15,
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(spherical_spiral_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  spiral_cluster)

  df

}

## Data structure 21

four_clust_21 <- function(n = 1500, triangle_vertices) {

  curvilinear_cluster <- gen_curv_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 4/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n * 2/15,
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  rect_corn_cluster,
                  gau_cluster,
                  nonlinear_cluster)

  df

}

## Data structure 22

four_clust_22 <- function(n = 1500, triangle_vertices) {

  nonlinear_cluster <- gen_s_curve_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 2/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  tri_corn_cluster,
                  cube_cluster,
                  blunted_corn_cluster)

  df

}

## Data structure 23

four_clust_23 <- function(n = 1500, triangle_vertices) {

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n * 6/15,
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 3,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 3/15,
    radius = 1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n * 2/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(nonlinear_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  hyperbola_cluster)

  df

}

## Data structure 24

four_clust_24 <- function(n = 1500, triangle_vertices) {

  curvilinear_cluster <- gen_curv2_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 3/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 2/15,
    radius = 1,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  hemisphere_cluster)

  df

}

## Data structure 25

four_clust_25 <- function(n = 1500, triangle_vertices) {

  hyperbola_cluster <- gen_nonlinear_hyperbola_4d(
    n = n * 6/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  rect_corn_cluster <- gen_corn_cluster_rectangular_base_4d(
    n = n * 4/15,
    height = 3,
    base_width_x = 2,
    base_width_y = 1,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  gau_cluster <- gen_gaussian_cluster_4d(
    n = n * 3/15,
    mean_vec = c(0, 0, 0, 0),
    cov_mat = diag(4) * 0.1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 2/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster,
                  rect_corn_cluster,
                  gau_cluster,
                  elliptical_cluster)

  df

}

## Data structure 26

four_clust_26 <- function(n = 1500, triangle_vertices) {

  curvilinear_cluster <- gen_crescent_4d(
    n = n * 6/15,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  tri_corn_cluster <- gen_corn_cluster_triangular_base_4d(
    n = n * 4/15,
    height = 5,
    base_width = 3,
    tip_radius = 0.5,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  cube_cluster <- gen_cube_4d(
    n = n * 3/15,
    side_length = 1,
    center_point = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  helical_cluster <- gen_helical_hyper_spiral_4d(
    n = n * 2/15,
    a = 0.1,
    b = 0.1,
    k = 2,
    spiral_radius = 1,
    scale_factor = 0.5,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(curvilinear_cluster,
                  tri_corn_cluster,
                  cube_cluster,
                  helical_cluster)

  df

}

## Data structure 27

four_clust_27 <- function(n = 1500, triangle_vertices) {

  hyperbola_cluster <- gen_nonlinear_hyperbola2_4d(
    n = n * 6/15,
    C = 1,
    nonlinear_factor = 0.5,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 3,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  hemisphere_cluster <- gen_hemisphere_4d(
    n = n * 3/15,
    radius = 1,
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  nonlinear_cluster <- gen_curvy_cylinder_4d(
    n = n * 2/15,
    radius = 1,
    height = 10,
    curve_strength = 1,
    offset = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(hyperbola_cluster,
                  hex_pyr_cluster,
                  hemisphere_cluster,
                  nonlinear_cluster)

  df

}

## Data structure 28

four_clust_28 <- function(n = 1500, triangle_vertices) {

  spiral_cluster <- gen_conic_spiral_4d(
    n = n * 6/15,
    spiral_turns = 1,
    cone_height = 2,
    cone_radius = 0.5,
    offset = triangle_vertices[1, ]
  ) |>
    mutate(cluster = "cluster1")

  blunted_corn_cluster <- gen_blunted_corn_cluster_4d(
    n = n * 4/15,
    height = 5,
    base_radius = 1.5,
    tip_radius = 0.8,
    tip_point = triangle_vertices[2, ]
  ) |>
    mutate(cluster = "cluster2")

  elliptical_cluster <- gen_elliptical_cluster_4d(
    n = n * 3/15,
    axes_lengths = c(2, 1.5, 1, 0.5),
    offset = triangle_vertices[3, ]
  ) |>
    mutate(cluster = "cluster3")

  hex_pyr_cluster <- gen_filled_hexagonal_pyramid_4d(
    n = n * 2/15,
    height = 5,
    base_radius = 3,
    tip_point = triangle_vertices[4, ]
  ) |>
    mutate(cluster = "cluster4")

  df <- bind_rows(spiral_cluster,
                  blunted_corn_cluster,
                  elliptical_cluster,
                  hex_pyr_cluster)

  df

}

