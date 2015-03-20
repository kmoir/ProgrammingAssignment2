## Put comments here that give an overall description of what your
## functions do

## defines functions to create a cache of the inverse 
## of a matrix along with getters and setters
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  getevn<- function() environment()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## checks if the the matrix inverse is already cached
## by calling getinverse
## if so it returns the cached version
## if not, it calls setinverse to create a new cached version 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

