# gen_circle() works

    Code
      gen_circle(n = 500, p = 4)
    Message
      v Data generation completed successfully! 🎉
    Output
      # A tibble: 500 x 4
              x1     x2     x3      x4
           <dbl>  <dbl>  <dbl>   <dbl>
       1  0.831  -0.556 -0.138  0.0974
       2 -0.906  -0.423 -0.522 -0.470 
       3 -0.0652  0.998  0.634  0.330 
       4 -0.621   0.784  0.344  0.0576
       5  0.816   0.578  0.599  0.493 
       6  0.358  -0.934 -0.513 -0.204 
       7  0.811  -0.585 -0.163  0.0800
       8 -0.977   0.215 -0.124 -0.269 
       9 -0.602  -0.798 -0.684 -0.495 
      10 -0.932  -0.362 -0.489 -0.458 
      # i 490 more rows

# gen_curvycycle() works

    Code
      gen_curvycycle(n = 500, p = 4)
    Message
      v Data generation completed successfully! 🎉
    Output
      # A tibble: 500 x 4
              x1       x2      x3      x4
           <dbl>    <dbl>   <dbl>   <dbl>
       1  0.831   0.0216  -0.0653  0.138 
       2 -0.906   0.154   -0.0854 -0.665 
       3 -0.0652  1.58     0.0648  0.466 
       4 -0.621   1.36     0.302   0.0814
       5  0.816   1.16    -0.0920  0.697 
       6  0.358  -0.356   -0.297  -0.288 
       7  0.811  -0.00753 -0.0996  0.113 
       8 -0.977   0.792   -0.266  -0.381 
       9 -0.602  -0.221    0.311  -0.700 
      10 -0.932   0.215   -0.148  -0.647 
      # i 490 more rows

# gen_unifsphere() works

    Code
      gen_unifsphere(n = 500, p = 4)
    Message
      v 1 noise dimensions have been generated successfully! 🎉
      v Data generation completed successfully! 🎉
    Output
      # A tibble: 500 x 4
              x1     x2      x3      x4
           <dbl>  <dbl>   <dbl>   <dbl>
       1 -0.476   0.337  0.812  -0.0495
       2 -0.0481 -0.989  0.139   0.0573
       3  0.412   0.775 -0.479  -0.0394
       4 -0.775  -0.563 -0.287  -0.0194
       5  0.208  -0.557 -0.804   0.0367
       6 -0.472   0.630  0.617  -0.0465
       7 -0.241  -0.548  0.801  -0.0129
       8  0.0913  0.993 -0.0689  0.0118
       9 -0.664   0.688  0.294  -0.0183
      10  0.878  -0.463  0.118   0.0315
      # i 490 more rows

# gen_gridedsphere() works

    Code
      gen_gridedsphere(n = 500, p = 4)
    Message
      v 1 noise dimensions have been generated successfully! 🎉
      v Data generation completed successfully! 🎉
    Output
      # A tibble: 91 x 4
             x1        x2    x3       x4
          <dbl>     <dbl> <dbl>    <dbl>
       1  0      0          1   -0.00877
       2  0      0          1    0.0184 
       3  0      0          1   -0.0436 
       4  0      0          1    0.00432
       5  0.866  0          0.5 -0.00742
       6 -0.433  7.5 e- 1   0.5  0.0116 
       7 -0.433 -7.5 e- 1   0.5  0.0473 
       8  0.866 -2.12e-16   0.5 -0.0125 
       9  0.866  0         -0.5  0.00392
      10 -0.433  7.5 e- 1  -0.5 -0.0722 
      # i 81 more rows

# gen_clusteredspheres() works

    Code
      clusteredspheres
    Output
      # A tibble: 1,300 x 5
            x1      x2      x3       x4 cluster
         <dbl>   <dbl>   <dbl>    <dbl> <chr>  
       1 -1.60  -0.781   1.61   1.99    small_3
       2 -1.52   2.60   -1.79   1.91    small_3
       3 10.5    9.92   -4.15   0.0775  big    
       4  9.65  -0.616  11.5   -0.0656  big    
       5  7.72   7.86   10.2   -0.0309  big    
       6 11.4   -8.40    4.92   0.00337 big    
       7 -5.17   6.72  -12.4    0.0173  big    
       8  6.96  11.5     6.68   0.0151  big    
       9 10.4  -10.8    -0.811  0.0429  big    
      10  5.69   5.14  -12.9    0.0734  big    
      # i 1,290 more rows

# gen_hemisphere() works

    Code
      gen_hemisphere(n = 500, p = 4)
    Message
      v Data generation completed successfully! 🎉
    Output
      # A tibble: 500 x 4
              x1    x2      x3      x4
           <dbl> <dbl>   <dbl>   <dbl>
       1  0.0880 0.277 -0.239  -0.927 
       2 -0.673  0.707 -0.215  -0.0291
       3  0.625  0.376  0.670   0.134 
       4 -0.278  0.856  0.0654  0.430 
       5 -0.249  0.173  0.316   0.899 
       6  0.253  0.507 -0.438  -0.698 
       7 -0.168  0.257 -0.496  -0.812 
       8  0.734  0.670  0.102   0.0342
       9  0.350  0.824 -0.417  -0.158 
      10 -0.954  0.236 -0.0718 -0.170 
      # i 490 more rows

