# Description:
#   Example of a distance function.

#' @import Rllvm
#' @import RLLVMCompile
NULL

# TODO:
#   * Map math functions to calls in libm (we already do for `pow`).
#   * Automatically get vector lengths.

# FIXME: Compiler should translate abs() -> fabs().
fabs = abs

#' Compile the functions for the distance example.
#'
#' @param module an existing LLVM module to use for compilation, optional
#' @return An LLVM module containing the compiled functions.
#' @export
compile_distance = function(module = Module())
{
  # Minkowski function.
  param_types = list(DoublePtrType, DoublePtrType, Int32Type, DoubleType)
  routines = list(
    fabs = list(DoubleType, DoubleType)
  )

  compileFunction(.minkowski, DoubleType, param_types, .routineInfo = routines,
    module = module)

  # Distance function.
  param_types = list(DoublePtrType, DoublePtrType, Int32Type, Int32Type,     
    Int32Type, DoubleType)

  compileFunction(.distance, REALSXPType, types = param_types, module = module)

  return(module)
}


#' Compute the Minkowski distance between two vectors.
#'
#' @name minkowski
#' @param x numeric, first vector
#' @param y numeric, second vector
#' @param len integer, length of x and y
#' @param p numeric, Minkowski parameter (e.g., 2 gives Euclidean distance)
.minkowski = function(x, y, len, p)
{
  # FIXME: Unless we assign a non-integer value first, the compiler will mark
  # `result` as an integer.
  result = 1.1
  result = 0

  for (i in 1:len) {
    sum = fabs(x[i] - y[i])^p
    result = result + sum
  }

  return(result^(1/p))
}


#' Compute distances from columns of x to columns of y.
#'
#' This version is non-idiomatic to suit the compiler's quirks, and should
#' not be called directly in the R interpreter.
#'
#' @name distance
#' @param x numeric, first matrix
#' @param y numeric, second matrix
#' @param nrow integer, number of rows in x (or y)
#' @param ncol_x integer, number of columns in x
#' @param ncol_y integer, number of columns in y
#' @param p numeric, Minkowski parameter (e.g., 2 gives Euclidean distance)
.distance = function(x, y, nrow, ncol_x, ncol_y, p)
{
  # Allocate a vector. Use column-major order.
  distances = numeric(ncol_x * ncol_y)
  x_vec = numeric(nrow)
  y_vec = numeric(nrow)

  for (i in 1:ncol_x) {
    for (j in 1:ncol_y) {

      for (k in 1:nrow) {
        id1 = (i - 1L) * nrow + k
        x_vec[k] = x[id1]

        id2 = (j - 1L) * nrow + k
        y_vec[k] = y[id2]
      }

      ids = (j - 1L) * ncol_x + i
      distances[ids] = .minkowski(REAL(x_vec), REAL(y_vec), nrow, p)
    }
  }

  return(distances)
}


.distance2 = function(x, y, nrow, ncol_x, ncol_y, p)
    # Compute distances from columns of x to columns of y.
{
  # TODO: detect call to matrix in compiler and allocate memory for an
  # appropriately sized array.
  distances = matrix(0, ncol_x, ncol_y)

  for (i in 1:ncol_x) {
    for (j in 1:ncol_y) {
      distances[i, j] = .minkowski(x[, i], y[, j], nrow, p)
    }
  }

  return(distances)
}
