# gen_noisedims() works

    Code
      gen_noisedims(n = 500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2))
    Message
      v 4 noise dimensions have been generated successfully! 🎉
    Output
      # A tibble: 500 x 4
             x1     x2     x3     x4
          <dbl>  <dbl>  <dbl>  <dbl>
       1 -2.64   1.98   2.29  -0.543
       2  1.28  -2.29  -2.56   2.06 
       3  2.58   1.58   2.60  -2.39 
       4 -2.57   0.775 -0.428  4.20 
       5 -0.755 -1.47  -2.52   2.05 
       6  0.308  1.86   2.22   0.980
       7  3.06   0.516  1.25   2.96 
       8 -0.781 -0.472  0.191  1.23 
       9  0.929  0.730  2.60   1.14 
      10 -5.96  -1.26  -2.23   0.355
      # i 490 more rows

# gen_bkgnoise() works

    Code
      gen_bkgnoise(n = 500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2))
    Message
      v Background noise generation completed successfully! 🎉
    Output
      # A tibble: 500 x 4
             x1     x2     x3     x4
          <dbl>  <dbl>  <dbl>  <dbl>
       1  2.64   1.98  -2.29  -0.543
       2 -1.28  -2.29   2.56   2.06 
       3 -2.58   1.58  -2.60  -2.39 
       4  2.57   0.775  0.428  4.20 
       5  0.755 -1.47   2.52   2.05 
       6 -0.308  1.86  -2.22   0.980
       7 -3.06   0.516 -1.25   2.96 
       8  0.781 -0.472 -0.191  1.23 
       9 -0.929  0.730 -2.60   1.14 
      10  5.96  -1.26   2.23   0.355
      # i 490 more rows

# randomize_rows() works

    Code
      randomize_rows(mobiusgau)
    Output
      # A tibble: 1,000 x 4
              x1     x2       x3       x4
           <dbl>  <dbl>    <dbl>    <dbl>
       1 -0.727  -0.279 -0.182    0.00549
       2  0.469  -0.863 -0.138    0.0171 
       3  0.994  -0.172 -0.0132   0.0122 
       4  0.937   0.323  0.0532  -0.0325 
       5 -0.0671 -0.614  0.140    0.00120
       6 -0.749   0.117  0.191    0.0369 
       7  0.0209 -0.790 -0.0441   0.0959 
       8  0.687   0.220  0.00900 -0.0259 
       9  0.746   0.146  0.0135   0.0184 
      10  0.586  -0.211  0.0302  -0.0626 
      # i 990 more rows

# relocate_clusters() works

    Code
      relocate_clusters(data = df, vert_mat = vert_mat)
    Output
      # A tibble: 12 x 5
              x1      x2      x3     x4 cluster
           <dbl>   <dbl>   <dbl>  <dbl>   <int>
       1 -1.18    1.62    5.14   -1.38        3
       2 -1.30    5.05   -0.0169  1.58        2
       3  3.54   -0.523  -0.131   0.433       1
       4  6.15    0.241   0.106  -0.330       1
       5  0.607   4.33   -0.700  -0.954       2
       6  0.620   5.63   -0.717   0.944       2
       7  2.27   -0.712   5.45    0.692       3
       8 -0.352   0.133   5.05   -0.293       3
       9 -0.736  -1.04    4.36    0.983       3
      10  0.0754  4.99    1.43   -1.57        2
      11  6.12    0.367  -0.106  -0.990       1
      12  4.19   -0.0853  0.131   0.887       1

# gen_nproduct() works

    Code
      gen_nproduct(7, 2)
    Output
      [1] 2 3

# gen_nsum() works

    Code
      gen_nsum(30, 3)
    Output
      [1] 10 10 10

# gen_wavydims1() works

    Code
      gen_wavydims1(n = 500, p = 4, theta = seq(pi / 6, 12 * pi / 6, length.out = 500))
    Message
      v Wavy shaped noise dimensions generation completed successfully! 🎉
    Output
      # A tibble: 500 x 4
              x1    x2      x3    x4
           <dbl> <dbl>   <dbl> <dbl>
       1  0.598   2.11  0.330   1.17
       2  0.419   1.43  0.920   1.30
       3  0.0740  1.80  0.409   2.02
       4  0.683   2.01  0.551   2.39
       5  0.531   2.32  1.29    2.12
       6  1.30    1.96  0.556   2.09
       7  1.23    2.15  0.937   2.08
       8 -0.330   1.72  1.47    1.79
       9  1.29    1.78  0.695   2.56
      10 -0.125   2.62 -0.0498  1.75
      # i 490 more rows

# gen_wavydims2() works

    Code
      gen_wavydims2(n = 500, p = 4, x1_vec = x1)
    Message
      v Wavy shaped noise dimensions generation completed successfully! 🎉
    Output
      # A tibble: 500 x 4
                x1        x2       x3       x4
             <dbl>     <dbl>    <dbl>    <dbl>
       1  0.000910  0.0185    0.0143  -0.0331 
       2 -0.00919  -0.00517   0.0170   0.0629 
       3  0.0186    0.00205  -0.00405 -0.00339
       4  0.0222    0.0312    0.00931 -0.00348
       5  0.00516   0.0139    0.00406 -0.00304
       6  0.0123    0.0238    0.00255  0.0591 
       7  0.00881   0.0309    0.00899  0.0691 
       8  0.00412   0.00188   0.0107  -0.0226 
       9  0.00292  -0.00113   0.0154   0.0264 
      10 -0.0106   -0.000241 -0.00240  0.0336 
      # i 490 more rows

# gen_wavydims3() works

    Code
      gen_wavydims3(n = 500, p = 4, data = df)
    Message
      v Wavy shaped noise dimensions generation completed successfully! 🎉
    Output
      # A tibble: 500 x 4
             x1    x2      x3    x4
          <dbl> <dbl>   <dbl> <dbl>
       1 -0.643 0.797 -1.78       0
       2  0.602 1.47  -0.199      0
       3 -0.771 0.353  1.63       0
       4 -0.983 1.21   0.789      0
       5  0.598 1.61   1.79       0
       6  0.232 0.706 -1.98       0
       7 -0.584 1.37  -1.81       0
       8 -0.314 0.468  0.0555     0
       9  0.974 0.752 -0.825      0
      10  0.531 1.84  -0.149      0
      # i 490 more rows

# gen_rotation() works

    Code
      gen_rotation(p = 4, planes_angles = rotations_4d)
    Output
                [,1]       [,2]         [,3]          [,4]
      [1,] 0.5000000 -0.8660254 0.000000e+00  0.000000e+00
      [2,] 0.8660254  0.5000000 0.000000e+00  0.000000e+00
      [3,] 0.0000000  0.0000000 6.123234e-17 -1.000000e+00
      [4,] 0.0000000  0.0000000 1.000000e+00  6.123234e-17

# normalize_data() works

    Code
      normalize_data(data = gau_data)
    Output
      # A tibble: 500 x 4
              x1      x2      x3       x4
           <dbl>   <dbl>   <dbl>    <dbl>
       1  0.369  -0.180  -0.362   0.360  
       2  0.106  -0.0432 -0.429   0.109  
       3 -0.130   0.835   0.101  -0.00610
       4  0.0711 -0.0203 -0.143   0.107  
       5 -0.0873  0.0955  0.114   0.275  
       6  0.375  -0.279  -0.0424 -0.372  
       7  0.0974  0.104   0.0309  0.0380 
       8 -0.246   0.352  -0.0548 -0.251  
       9  0.102   0.187   0.0759 -0.119  
      10 -0.0608  0.280   0.153  -0.246  
      # i 490 more rows

# gen_clustloc() works

    Code
      gen_clustloc(p = 4, k = 3)
    Output
                 [,1]       [,2]        [,3]
      [1,]  1.8727238 -0.7444274 -1.12829641
      [2,] -0.9968225  0.9259866  0.07083593
      [3,]  0.7820890 -0.1252220 -0.65686699
      [4,]  1.5232486 -0.4366755 -1.08657315

