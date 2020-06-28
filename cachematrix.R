## These functions create and store a matrix then calculate and cache the inverse

## Function creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) c <<- inverse
  getinverse <- function() c
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse from the above matrix or retrieves it from cache if the
## same has already been stored.

cacheSolve <- function(x, ...) {
  c <- x$getinverse()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data,...)
  x$setinverse(c)
  ## Return a matrix that is the inverse of 'x'
  c
}
