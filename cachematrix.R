## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  revm <- NULL
  setm <- function(y) {
    m <<- y
    revm <- NULL
  }
  getm <- function() m
  setrevm <- function(inverse) revm <<- inverse
  getrevm <- function() revm
  list(setmatrix = setm, getmatrix = getm, setrevmatrix = setrevm, getrevmatix = getrevm)
}


## Write a short comment describing this function
# a constructor function returning a named list of four functions:
# setting the initial matrix (stem)
# getting the initial matrix (getm) from the makeCacheMatrix's argument
# setting the inverse matrix (setrevm)
# getting the inverse matrix (getrevm)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mrev <- x$getrevmatix()
# checks if the inverse matrix is alredy saved as the fourth element of the set above and returns it
  if(!is.null(mrev)){
    message('The inverted matrix is cached:')
    return(mrev)
  }
# ... if not, computes the inverse matrix using the function solve(). 
# One can see that the latter process takes nore time than the former
  else {
    message('The inverted matrix is computed from scratch:')
    initmatrix = x$getmatrix()
    mrev = solve(initmatrix, ...)
  }
  x$setrevmatrix(mrev)
  return(mrev)
}
