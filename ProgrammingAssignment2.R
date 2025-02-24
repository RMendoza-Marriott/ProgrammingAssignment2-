### Programming assignment ####
### Data Science program - Coursera ###



### Function 1: This function creates a special 
### matrix object than can cache its
### inverse


makeCacheMatrix <- function(sample = matrix()) {
  invsample <- 0
  set <- function(x) {
    Sample <<- x
    invsample <<- 0
  }
  get <- function() sample
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() invsample
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function 2
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(sample, ...) {
  ## Return a matrix that is the inverse of 'sample'
  inv <- sample$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(invsample)
  }
  mat <- sample$get()
  invsample <- solve(mat, ...)
  sample$setInverse(invsample)
  invsample
}
