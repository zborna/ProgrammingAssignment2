## This function create a matrix and make inverse of the matrix
## available in the cache


## Create and return list of functions

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Calculation of inverse matrix, if calculation already stored in cache
## it returns chached value

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
  cache <- x$getInverse()
  if(!is.null(cache)) {
    message("getting cached data.")
    return(cache)
  }
  matrix <- x$get()
  cache <- solve(matrix, ...)
  x$setInverse(cache)
  return (cache)
  
}
