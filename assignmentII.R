## I set the input x as a matrix
## and then set the solved value "inv" as a null
## then I changed every reference of "mean" to "solve"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
## Same here, changed "mean" to "solve" and "m" to "mat"
## This function computes the inverse of the special "matrix" created by makeCacheMatrix above.
##If the inverse has already been calculated (and the  matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
## here in this execution, the functions will create object which stores a matrix and caches its inverse.