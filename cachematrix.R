##
## The two functions in this file support the caching of the Inverse of a Matrix
## to reduce calculation time
##
## To test, run the following commands:
## > m <- matrix(c(1,3,2,4), 2, 2) # creates a 2-by-2 matrix
## > matr <- makeCacheMatrix(m)    # generates the list of functions
## > cacheSolve(matr)              # creates and returns the inverse matrix
## > cacheSolve(matr)              # second call returns the cached matrix
## > n <- matrix(c(6,5,2,4), 2, 2) # matrix to replace m
## > matr$set(n)                   # replace m inside the list
## > cacheSolve(matr)              # recalculates the inverse due to change

## Function makeCacheMatrix
##
## This function creates and returns a list with a set of functions needed
## to store and retrieve a matrix and its inverse:
## - get: returns the original matrix
## - set: changes the matrix value and invalidates the cached inverse
## - setinverse: calculates the inverse of the matrix using the function 'solve'
##               and stores it in a specific environment
## - getinverse: returns the inverse of the original matrix
##
## Usage: makeCacheMatrix(x)
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function cacheSolve
##
## This function returns the inverse of the matrix stored inside list 'x'.
## It can either return a cached value or calculate the inverse if the matrix
## has changed since the last calculation was made.
##
## Usage: cacheSolve(x)
##

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
