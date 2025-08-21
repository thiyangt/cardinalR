# gen_crescent() works

    Code
      gen_crescent(n = 500, p = 4)
    Message
      v Wavy shaped noise dimensions generation completed successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
            x1    x2    x3    x4
         <dbl> <dbl> <dbl> <dbl>
       1 0.866 0.5   0.340  2.43
       2 0.860 0.510 0.971  2.00
       3 0.854 0.520 0.503  1.45
       4 0.848 0.530 0.632  2.23
       5 0.842 0.539 0.453  1.55
       6 0.836 0.549 0.109  1.91
       7 0.829 0.559 0.717  2.13
       8 0.823 0.568 0.565  2.44
       9 0.816 0.578 1.34   2.08
      10 0.810 0.587 1.27   2.27
      # i 490 more rows

# gen_curvycylinder() works

    Code
      gen_curvycylinder(n = 500, p = 4, h = 10)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
             x1     x2    x3     x4
          <dbl>  <dbl> <dbl>  <dbl>
       1 -0.634  0.773  4.02 -0.770
       2  0.610 -0.793  7.42  0.908
       3 -0.773  0.635  1.72  0.988
       4 -0.976 -0.218  6.00 -0.280
       5  0.602  0.799  8.07  0.977
       6  0.234  0.972  3.52 -0.373
       7 -0.592  0.806  6.84  0.528
       8 -0.319 -0.948  2.35  0.709
       9  0.983 -0.183  3.72 -0.548
      10  0.527 -0.850  9.23  0.196
      # i 490 more rows

# gen_sphericalspiral() works

    Code
      gen_sphericalspiral(n = 500, p = 4, spins = 1)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
              x1        x2    x3      x4
           <dbl>     <dbl> <dbl>   <dbl>
       1 0       0         1.41  0      
       2 0.00630 0.0000793 1.07  0.00200
       3 0.0126  0.000317  0.760 0.00401
       4 0.0189  0.000713  0.856 0.00601
       5 0.0251  0.00127   0.598 0.00802
       6 0.0314  0.00198   1.31  0.0100 
       7 0.0377  0.00285   1.40  0.0120 
       8 0.0439  0.00388   0.965 0.0140 
       9 0.0501  0.00506   1.15  0.0160 
      10 0.0563  0.00640   1.06  0.0180 
      # i 490 more rows

# gen_helicalspiral() works

    Code
      gen_helicalspiral(n = 500, p = 4)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
            x1      x2      x3       x4
         <dbl>   <dbl>   <dbl>    <dbl>
       1 1     0        0.406  0       
       2 1.000 0.00787  0.0700 0.000787
       3 1.000 0.0157  -0.239  0.00157 
       4 1.000 0.0236  -0.142  0.00236 
       5 1.000 0.0315  -0.400  0.00315 
       6 0.999 0.0393   0.310  0.00393 
       7 0.999 0.0472   0.403  0.00472 
       8 0.998 0.0551  -0.0317 0.00551 
       9 0.998 0.0629   0.150  0.00629 
      10 0.997 0.0708   0.0625 0.00708 
      # i 490 more rows

# gen_conicspiral() works

    Code
      gen_conicspiral(n = 500, p = 4, spins = 1)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
             x1       x2      x3     x4
          <dbl>    <dbl>   <dbl>  <dbl>
       1 0      0         0.534  0.181 
       2 0.0126 0.000159  0.303  0.420 
       3 0.0252 0.000634  0.0903 0.0219
       4 0.0377 0.00143   0.162  0.323 
       5 0.0503 0.00254  -0.0152 0.470 
       6 0.0628 0.00396   0.486  0.155 
       7 0.0753 0.00570   0.554  0.390 
       8 0.0878 0.00776   0.254  0.0802
       9 0.100  0.0101    0.385  0.181 
      10 0.113  0.0128    0.327  0.571 
      # i 490 more rows

# gen_nonlinear() works

    Code
      gen_nonlinear(n = 500, p = 4, hc = 1, non_fac = 0.5)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
            x1    x2    x3     x4
         <dbl> <dbl> <dbl>  <dbl>
       1 1.82   1.03 0.381  0.915
       2 1.18   1.31 0.620 -0.923
       3 0.595  1.96 0.221 -0.368
       4 0.778  1.64 0.520 -0.685
       5 0.287  3.63 0.665  0.678
       6 1.64   1.11 0.347  0.442
       7 1.81   1.04 0.579  0.859
       8 0.985  1.43 0.265 -1.06 
       9 1.33   1.24 0.361 -0.564
      10 1.16   1.32 0.746 -0.824
      # i 490 more rows

