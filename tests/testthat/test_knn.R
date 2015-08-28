# Description:
#   Tests for the knn example.

test_that("compiled order function is correct", {
  module = compile_knn()
  .order = module[[".order"]]

  x = c(1.3, -1.2, 2.1, 3.5, 7.1, -20, 4)
  result = .llvm(.order, x, length(x))
  target = order(x)
  expect_equal(result, target)

  x = c(-0.33, 2.02, -1.7, -1.01, -0.99, -0.79, -1.59) 
  result = .llvm(.order, x, length(x))
  target = order(x)
  expect_equal(result, target)
})


test_that("compiled which.max function is correct", {
  module = compile_knn()
  .which.max = module[[".which.max"]]

  x = c(1L, 3L, 15L, 3L)
  result = .llvm(.which.max, x, length(x))
  target = which.max(x)
  expect_equal(result, target)

  x = c(5L, 9L, 10L, 3L, 8L, 12L, 5L, 11L)
  result = .llvm(.which.max, x, length(x))
  target = which.max(x)
  expect_equal(result, target)

  x = -c(1L, 2L, 3L, 4L)
  result = .llvm(.which.max, x, length(x))
  target = which.max(x)
  expect_equal(result, target)
})


test_that("compiled knn function is correct", {
  module = compile_knn()
  knn = module[[".knn"]]

  distances = matrix(c(0.5, 0.72, 1.5, 23.1, 12.3, 14.1, 0.2, 1.9), 4, 2)
  labels = c(1L, 1L, 2L, 2L)

  result = .llvm(knn, distances, labels, 2L, 2L, nrow(distances),
    ncol(distances))
  target = c(1L, 2L)
  expect_equal(result, target)
})


test_that("compiled knn function is correct for iris data", {
  # This is a "realistic" test of 3-nearest neighbors on the iris data.
  distance = compile_distance()[[".distance"]]
  knn = compile_knn()[[".knn"]]

  set.seed(40)

  train_idx = sample.int(nrow(iris), 50)
  train = iris[train_idx, 1:4]
  test = iris[-train_idx, 1:4]
  labels = as.integer(iris$Species)[train_idx]

  # Computed with:
  #
  #   target = class::knn(train, test, labels, 3L)
  #
  target = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L,
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L,
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L,
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 3L, 2L, 3L,
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L)

  train = t(train)
  test = t(test)
  distances = .llvm(distance, train, test, nrow(train), ncol(train),
    ncol(test), 2L)

  result = .llvm(knn, distances, labels, 3L, 3L, ncol(train), ncol(test))
  
  expect_equal(result, target)
})
