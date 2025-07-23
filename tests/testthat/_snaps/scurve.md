# gen_scurve() works

    Code
      gen_scurve(n = 500, p = 4)
    Message
      v Wavy shaped noise dimensions generation completed successfully! 🎉
      v Data generation completed successfully! 🎉
    Output
      # A tibble: 500 x 4
             x1    x2      x3     x4
          <dbl> <dbl>   <dbl>  <dbl>
       1 -0.634 0.804 -1.77   -0.627
       2  0.610 1.48  -0.207   0.602
       3 -0.773 0.345  1.63   -0.780
       4 -0.976 1.20   0.782  -0.968
       5  0.602 1.61   1.80    0.607
       6  0.234 0.705 -1.97    0.237
       7 -0.592 1.37  -1.81   -0.589
       8 -0.319 0.471  0.0522 -0.325
       9  0.983 0.744 -0.817   0.978
      10  0.527 1.85  -0.150   0.532
      # i 490 more rows

# gen_scurvehole() works

    Code
      gen_scurvehole(n = 500, p = 4)
    Message
      v Data generation completed successfully! 🎉
      v 1 noise dimensions have been generated successfully! 🎉
      v Data generation completed successfully! 🎉
    Output
      # A tibble: 480 x 4
             x1    x2      x3      x4
          <dbl> <dbl>   <dbl>   <dbl>
       1 -0.634 0.804 -1.77   -0.0495
       2  0.610 1.48  -0.207   0.0573
       3 -0.773 0.345  1.63   -0.0394
       4 -0.976 1.20   0.782  -0.0194
       5  0.602 1.61   1.80    0.0367
       6  0.234 0.705 -1.97   -0.0465
       7 -0.592 1.37  -1.81   -0.0129
       8 -0.319 0.471  0.0522  0.0118
       9  0.983 0.744 -0.817  -0.0183
      10  0.527 1.85  -0.150   0.0315
      # i 470 more rows

