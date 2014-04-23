#Caching the Inverse of a Matrix

## The set of functions below will: 
## -Check if the matrix inverse has been calculated
## -If so, bring it from cache
## -If not, calculate the inverse of the matrix
## -Cache the new result


## Creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Computes the inverse of the special matrix created by "makeCacheMatrix"
## checking if it has already been calculated previously.
## If it has, then simply retrieve the result from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
