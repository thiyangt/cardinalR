# gen_expbranches() works

    Code
      gen_expbranches(n = 400, k = 4)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 2
             x1      x2
          <dbl>   <dbl>
       1 -1.61  18.1   
       2  1.23   0.172 
       3  1.60   0.0831
       4 -0.138  1.28  
       5  0.588  0.434 
       6  0.236  0.710 
       7 -0.245  1.59  
       8 -0.368  2.00  
       9 -1.75  23.4   
      10 -1.31  10.6   
      # i 390 more rows

# gen_orgcurvybranches() works

    Code
      gen_orgcurvybranches(n = 400, p = 4, k = 4)
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 4
               x1       x2    x3      x4
            <dbl>    <dbl> <dbl>   <dbl>
       1 -0.140   -0.0537  1.29  -2.08  
       2  0.0553  -0.0138  1.12  -1.59  
       3 -0.0447  -0.0437  0.877 -0.966 
       4 -0.0402  -0.0389  0.816 -0.673 
       5  0.00297 -0.00582 0.125  0.261 
       6 -0.0240   0.0174  0.344  0.0121
       7  0.0514   0.00884 1.30  -2.51  
       8 -0.0438   0.0173  1.20  -1.80  
       9  0.0609  -0.00164 0.642 -0.540 
      10 -0.00698  0.0421  0.937 -1.14  
      # i 390 more rows

# gen_orglinearbranches() works

    Code
      gen_orglinearbranches(n = 400, p = 4, k = 4)
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 4
               x1       x2    x3      x4
            <dbl>    <dbl> <dbl>   <dbl>
       1 -0.140   -0.0537  1.29  -1.50  
       2  0.0553  -0.0138  1.12  -1.40  
       3 -0.0447  -0.0437  0.877 -1.13  
       4 -0.0402  -0.0389  0.816 -0.898 
       5  0.00297 -0.00582 0.125  0.0966
       6 -0.0240   0.0174  0.344 -0.327 
       7  0.0514   0.00884 1.30  -1.92  
       8 -0.0438   0.0173  1.20  -1.45  
       9  0.0609  -0.00164 0.642 -0.884 
      10 -0.00698  0.0421  0.937 -1.23  
      # i 390 more rows

# gen_linearbranches() works

    Code
      gen_linearbranches(n = 400, k = 4)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 2
             x1      x2
          <dbl>   <dbl>
       1  7.06   4.03  
       2  3.70   2.15  
       3  0.604  0.600 
       4  1.57   1.06  
       5 -1.02  -0.0783
       6  6.08   3.36  
       7  7.01   3.64  
       8  2.66   1.33  
       9  4.47   2.67  
      10  3.59   2.07  
      # i 390 more rows

# gen_curvybranches() works

    Code
      gen_curvybranches(n = 400, k = 4)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 2
             x1     x2
          <dbl>  <dbl>
       1 0.906  0.962 
       2 0.570  0.411 
       3 0.260  0.124 
       4 0.357  0.191 
       5 0.0982 0.0625
       6 0.808  0.766 
       7 0.901  0.915 
       8 0.466  0.264 
       9 0.647  0.527 
      10 0.559  0.396 
      # i 390 more rows

