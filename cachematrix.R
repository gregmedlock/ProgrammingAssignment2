## makeCacheMatrix and cacheSolve compute the inverse of a matrix and allow
## efficient recall by storing the solution in a cache.

## Creates a list of functions for computing the inverse of a matrix 'x'.
## Functions in returned list are:
## getMatrix(): returns the stored matrix
## setMatrix(y): assigns matrix 'y' to the stored matrix variable 'x'
## getSolved(): returns the previously computed inverse
## solve(): finds the inverse and returns the solution
makeCacheMatrix <- function(x = matrix()) {
  solution <- NULL
  setMatrix <- function(y) {
    x <<- y
    solution <<- NULL #set the solution to null since we're modifying the matrix
  }
  getMatrix <- function() x
  invert <- function(newSolution) solution <<- newSolution
  getSolved <- function() solution
  list(getMatrix = getMatrix, setMatrix = setMatrix,
       invert = invert,
       getSolved = getSolved)
}

## Returns the inverse of matrix contained in x$getMatrix().
## If solution exists in x$getSolved(), returns cached solution instead.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  solution <- x$getSolved()
  if (!is.null(solution)) {
    message("Getting cached data")
    return(solution)
  }
  matrix <- x$getMatrix()
  solution <- solve(matrix,...)
  x$invert(solution)
  solution
}
