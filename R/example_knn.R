# Description:
#   Example of k-nearest neighbors.

#' Compile the functions for the knn example.
#'
#' @param module an existing LLVM module to use for compilation, optional
#' @return An LLVM module containing the compiled functions.
#' @export
compile_knn = function(module = Module())
{
  # Order function
  param_types = list(DoublePtrType, Int32Type)
  compileFunction(.order, INTSXPType, types = param_types, module = module)

  # which.max function
  param_types = list(Int32PtrType, Int32Type)
  compileFunction(.which.max, Int32Type, types = param_types, module = module)

  # knn function
  param_types = list(DoublePtrType, Int32PtrType, Int32Type, Int32Type,
    Int32Type, Int32Type)
  compileFunction(.knn, REALSXPType, types = param_types, module = module)

  return(module)
}


#' Compute an index into x which specifies the elements in ascending order.
#'
#' This is a clone of R's `order()` function.
#'
#' @name order
#' @param x numeric, vector of values
#' @param len integer, length of x
.order = function(x, len)
{
  idx = integer(len)
  idx[1] = 1L

  for (insert_idx in 2L:len) { # each number
    # TODO:
    #   The compiler generates a lot of unnecessary `load` instructions here.
    #   Also, calls to `sub` with constants are not consolidated.
    idx[insert_idx] = insert_idx
    insert_val = x[insert_idx]

    # Working from the right edge, shift elements of idx right until the
    # index is at the correct insertion point.
    # TODO:
    #   The compiler doesn't support descending for loops, so we fake it here
    #   with a while loop.
    j = insert_idx
    while(j > 1L) {
      active_idx = idx[j - 1L]
      active_val = x[active_idx]

      if(insert_val < active_val) {
        idx[j] = active_idx
      } else {
        break
      }

      j = j - 1L
    }
    idx[j] = insert_idx
  }

  return(idx)
}


#' Compute index of max element.
#'
#' This is a clone of R's `which.max()`.
#'
#' @name which.max
#' @param x integer, vector of values
#' @param len numeric, length of x
.which.max = function(x, len)
{
  max_idx = 1L
  max = x[1L]

  for (i in 2L:len) {
    if (x[i] > max) {
      max_idx = i
      max = x[i]
    }
  }

  return(max_idx)
}


#' Classify points using k-nearest neighbors.
#'
#' This version is non-idiomatic to suit the compiler's quirks, and should
#' not be called directly in the R interpreter.
#'
#' @name knn
#' @param distances numeric, distance matrix, where rows correspond to
#' training points and columns correspond to test points
#' @param labels integer, class labels starting at 1
#' @param n_class integer, number of classes
#' @param k integer, number of nearest neighbors to use
#' @param n_train integer, number of training points
#' @param n_test integer, number of test points
.knn = function(distances, labels, n_class, k, n_train, n_test)
{
  predictions = integer(n_test)
  counter = integer(n_class)
  current_distances = numeric(n_train)

  for (j in 1:n_test) { # each test observation
    # Zero the counter.
    for (i1 in 1:n_class) {
      counter[i1] = 0L
    }

    # Copy out distances for the current test observation.
    # TODO:
    #   The compiler should automatically do this for R's correpsonding subset
    #   operation, and if possible we should pass a pointer rather than a copy.
    for (i2 in 1L:n_train) {
      id = (j - 1L) * n_train + i2
      current_distances[i2] = distances[id]
    }

    # Order the current distances.
    order_idx = .order(REAL(current_distances), n_train)
    order_idx_int = INTEGER(order_idx)

    # Get the class labels for the k closest training points, and use them to
    # increment the counter.
    for (i3 in 1L:k) {
      ord = order_idx_int[i3]
      label = labels[ord]
      counter[label] = counter[label] + 1L
    }
    
    # Get class with the most votes.
    predictions[j] = .which.max(INTEGER(counter), n_class)
  }

  return(predictions)
}


.knn2 = function(test, train, labels, nclass, k)
  # k-nearest neighbors.
  #
  # Args:
  #   test    matrix of test points, one COLUMN per observation
  #   train   matrix of training ponits, one COLUMN per observation
  #   labels  class labels, as integers starting at 1
  #   nclass  number of classes
  #   k
{
    ncol_test = ncol(test)

    distances = distance(train, test, nrow(train), ncol(train), ncol_test, 2)

    # Allocate space for predictions and label counter.
    predictions = integer(ncol_test)
    counter = integer(nclass)

    # Columns correspond to test points, so sort distances in each column.
    for (j in 1:ncol_test) {
        # Zero the counter.
        for (i in 1:nclass) counter[i] = 0

        # TODO: A partial ordering algorithm would be more efficient here.
        order_idx = order(distances[, j])

        # Only consider the k nearest neighbors.
        for (i in 1:k) {
            label = labels[order_idx[i]]
            counter[label] = counter[label] + 1L
        }

        predictions[j] = which.max(counter)
    }

    return(predictions)
}
