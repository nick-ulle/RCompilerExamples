# Description:
#   Tests for the distance example.

test_that("compiled Minkowski function is correct", {
  module = compile_distance()
  minkowski = module[[".minkowski"]]

  x = c(0, 0)
  y = c(1, 1)
  p = 2
  result = .llvm(minkowski, x, y, length(x), p)
  target = as.numeric(dist(rbind(x, y), method = "minkowski", p = p))
  expect_equal(result, target)

  x = c(-1, -1)
  y = c(0.5, 0.5)
  p = 2
  result = .llvm(minkowski, x, y, length(x), p)
  target = as.numeric(dist(rbind(x, y), method = "minkowski", p = p))
  expect_equal(result, target)

  x = c(1.1, 2.1, -1.2)
  y = c(1.3, 1.5, 6.4)
  p = 10
  result = .llvm(minkowski, x, y, length(x), p)
  target = as.numeric(dist(rbind(x, y), method = "minkowski", p = p))
  expect_equal(result, target)
})


test_that("compiled distance function is correct", {
  module = compile_distance()
  distance = module[[".distance"]]

  r_distance = function(x, y, p)
    # Compute distance between columns of two matrices using base R functions.
  {
    stacked_points = rbind(t(x), t(y))
    d = dist(stacked_points, method = "minkowski", p = p)
    
    d = as.matrix(d)[1L:ncol(x), ncol(x) + 1L:ncol(y)]
    return(as.numeric(d))
  }

  x = matrix(c(1, 1, 0, 0), 2, 2)
  y = matrix(c(1, 1, 0, 0, 2, 2), 2, 3)
  p = 2
  result = .llvm(distance, x, y, nrow(x), ncol(x), ncol(y), p)
  target = r_distance(x, y, p)
  expect_equal(result, target)


  x = matrix(c(1.3, 305.1), 2, 1)
  y = matrix(c(1.34, 1.2, -5, -3.1, 2.2, -0.5, 200, -100), 2, 4)
  p = 7
  result = .llvm(distance, x, y, nrow(x), ncol(x), ncol(y), p)
  target = r_distance(x, y, p)
  expect_equal(result, target)

  x = matrix(c(1, 3, 9.5, 4.2, 0, 0, 0, 0), 4, 2)
  y = matrix(c(1, 1, 1, 1, -1, 0.3, 1, 0.3, 2, -2, 2, 2.1), 4, 3)
  p = 3
  result = .llvm(distance, x, y, nrow(x), ncol(x), ncol(y), p)
  target = r_distance(x, y, p)
  expect_equal(result, target)
})
