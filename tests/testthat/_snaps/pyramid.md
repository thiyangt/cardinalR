# gen_pyrrect() works

    Code
      gen_pyrrect(n = 500, p = 4, h = 5, l_vec = c(3, 2), rt = 0.5)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
              x1      x2       x3    x4
           <dbl>   <dbl>    <dbl> <dbl>
       1  0.283  -0.407   0.0201  0.451
       2 -0.605  -1.10   -0.0131  2.80 
       3  1.47    1.60    0       5    
       4 -1.22    0.0736 -0.0291  1.54 
       5  0.0396 -0.221  -0.0108  0.807
       6  0.540  -0.581   0.0804  0.295
       7 -0.658   0.626  -0.00691 1.84 
       8 -1.42   -1.78    0.00972 4.41 
       9  0.652  -0.271   0.0526  0.760
      10  0.517   0.114   0.0192  0.492
      # i 490 more rows

# gen_pyrtri() works

    Code
      gen_pyrtri(n = 500, p = 4, h = 5, l = 3, rt = 0.5)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
             x1     x2       x3    x4
          <dbl>  <dbl>    <dbl> <dbl>
       1 0.0909 0.504  -0.0279  0.451
       2 1.08   0.647  -0.0172  2.80 
       3 1.94   0.763   0       5    
       4 0.560  0.0270  0.0401  1.54 
       5 0.115  0.471   0.0721  0.807
       6 0.0498 0.594  -0.0807  0.295
       7 0.0937 1.04    0.00123 1.84 
       8 2.03   0.643  -0.00460 4.41 
       9 0.162  0.114   0.0159  0.760
      10 0.324  0.115   0.0777  0.492
      # i 490 more rows

# gen_pyrstar() works

    Code
      gen_pyrstar(n = 500, p = 4, h = 5, rb = 3)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
              x1        x2       x3    x4
           <dbl>     <dbl>    <dbl> <dbl>
       1 -0.114   1.39e-17 -0.00789 4.53 
       2  0.373  -6.46e- 1  0.0120  2.85 
       3 -0.608   1.05e+ 0  0.0317  1.30 
       4 -1.58    1.93e-16  0.00903 1.78 
       5 -0.856   1.48e+ 0 -0.0829  0.491
       6  0.132  -2.29e- 1  0.00529 4.04 
       7 -0.0679 -1.18e- 1 -0.00392 4.50 
       8 -0.206   3.57e- 1 -0.0372  2.33 
       9 -0.378  -6.55e- 1 -0.0205  3.24 
      10 -0.499   8.65e- 1 -0.0121  2.79 
      # i 490 more rows

# gen_pyrholes() works

    Code
      gen_pyrholes(n = 500, p = 3)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 3
               x1      x2      x3
            <dbl>   <dbl>   <dbl>
       1  0.807    0.489   0.275 
       2  0.0498   0.449   0.282 
       3  0.0249   0.224  -0.292 
       4  0.0124   0.112  -0.579 
       5  0.00622 -0.352  -0.145 
       6  0.357    0.0280  0.0717
       7  0.178    0.0140 -0.397 
       8 -0.264    0.211  -0.0542
       9 -0.132    0.106  -0.460 
      10  0.287    0.257  -0.0857
      # i 490 more rows

