library('testthat')

points_for_all_tests(c("r8"))

test("8a", c("r8.1"), {
  expected_A <- matrix(c(2, 4, 6, 8, 10, 12, 14, 16, 1, 3, 5, 7, 9, 11, 13, 15),
                       nrow = 4)
  expect_equal(A, expected_A)
  
  expected_B <- matrix(c(1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 3, 0, 0, 0, 0, 4),
                       nrow = 4)
  expect_equal(B, expected_B)
  
  expected_C <- matrix(c(2, 4, 6, 8, 20, 24, 28, 32, 3, 9, 15, 21, 36, 44, 52, 60),
                       nrow = 4)
  expect_equal(C, expected_C)
})

test("8b", c("r8.2"), {
  expected_D <- matrix(c(20, 24, 32, 3, 9, 21, 36, 44, 60), nrow = 3)
  expect_equal(D, expected_D)
})

test("8c", c("r8.3"), {
  expected_E <- matrix(c(20, 24, 32, 3, 9, 21, 36, 44, 60, 3, -2, 0), nrow = 3)
  expect_equal(E, expected_E)
})

test("8d", c("r8.4"), {
  expected_G <- matrix(c(20, 24, 32, -4, 3, 9, 21, 1, 36, 44, 60, 1, 3, -2, 0, 2),
                         nrow = 4)
  expect_equal(G, expected_G)
})

test("8e", c("r8.5"), {
  expected_H <- matrix(c(0.0668604651162791, -0.0465116279069767, -0.0193798449612403, 
                         0.166666666666667, -0.12296511627907, -0.0883720930232558, 
                         0.0965116279069767, -0.25, 0.0537790697674419, 0.0930232558139535,
                         -0.0445736434108527, 0.0833333333333333, -0.223255813953488,
                         -0.0186046511627907, 0.125581395348837, 0), nrow = 4)
  expect_equal(H, expected_H)
})
