# gen_expbranches() works

    Code
      gen_expbranches(n = 400, p = 4, k = 4)
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 800 x 4
            x1    x2    x3    x4
         <dbl> <dbl> <dbl> <dbl>
       1     0     0     0     0
       2     0     0     0     0
       3     0     0     0     0
       4     0     0     0     0
       5     0     0     0     0
       6     0     0     0     0
       7     0     0     0     0
       8     0     0     0     0
       9     0     0     0     0
      10     0     0     0     0
      # i 790 more rows

# gen_orgcurvybranches() works

    Code
      gen_orgcurvybranches(n = 400, p = 4, k = 4)
    Condition
      Warning:
      The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
      i Using compatibility `.name_repair`.
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 4
               x1      x2    x3      x4
            <dbl>   <dbl> <dbl>   <dbl>
       1 -0.0196  -0.270  0.196  0.392 
       2  0.0243   0.0607 1.62  -2.29  
       3 -0.280   -0.107  1.80  -3.11  
       4  0.111   -0.0277 0.931 -0.860 
       5 -0.0894  -0.0874 1.29  -1.24  
       6 -0.0804  -0.0779 1.12  -0.970 
       7  0.00594 -0.0116 0.877 -0.581 
       8 -0.0480   0.0348 0.816 -0.340 
       9  0.103    0.0177 0.125  0.269 
      10 -0.0875   0.0346 0.344  0.0714
      # i 390 more rows

# gen_orglinearbranches() works

    Code
      gen_orglinearbranches(n = 400, p = 4, k = 4)
    Condition
      Warning:
      The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
      i Using compatibility `.name_repair`.
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 4
               x1      x2    x3     x4
            <dbl>   <dbl> <dbl>  <dbl>
       1 -0.0196  -0.270  0.196  0.234
       2  0.0243   0.0607 1.62  -1.30 
       3 -0.280   -0.107  1.80  -1.66 
       4  0.111   -0.0277 0.931 -0.924
       5 -0.0894  -0.0874 1.29  -0.857
       6 -0.0804  -0.0779 1.12  -0.838
       7  0.00594 -0.0116 0.877 -0.689
       8 -0.0480   0.0348 0.816 -0.490
       9  0.103    0.0177 0.125  0.159
      10 -0.0875   0.0346 0.344 -0.154
      # i 390 more rows

# gen_linearbranches() works

    Code
      gen_linearbranches(n = 400, p = 4, k = 4)
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 4
             x1      x2       x3       x4
          <dbl>   <dbl>    <dbl>    <dbl>
       1  7.06   4.03    0.0927   0.0187 
       2  3.70   2.15   -0.0614  -0.0355 
       3  0.604  0.600  -0.00981 -0.135  
       4  1.57   1.06    0.0122   0.0304 
       5 -1.02  -0.0783 -0.140   -0.0537 
       6  6.08   3.36    0.0553  -0.0138 
       7  7.01   3.64   -0.0447  -0.0437 
       8  2.66   1.33   -0.0402  -0.0389 
       9  4.47   2.67    0.00297 -0.00582
      10  3.59   2.07   -0.0240   0.0174 
      # i 390 more rows

# gen_curvybranches() works

    Code
      gen_curvybranches(n = 400, p = 4, k = 4)
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 4
             x1     x2       x3       x4
          <dbl>  <dbl>    <dbl>    <dbl>
       1 0.906  0.962   0.0927   0.0187 
       2 0.570  0.411  -0.0614  -0.0355 
       3 0.260  0.124  -0.00981 -0.135  
       4 0.357  0.191   0.0122   0.0304 
       5 0.0982 0.0625 -0.140   -0.0537 
       6 0.808  0.766   0.0553  -0.0138 
       7 0.901  0.915  -0.0447  -0.0437 
       8 0.466  0.264  -0.0402  -0.0389 
       9 0.647  0.527   0.00297 -0.00582
      10 0.559  0.396  -0.0240   0.0174 
      # i 390 more rows

