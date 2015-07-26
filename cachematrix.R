## Cache the inverse of a matrix

## Create a matrix object that can cache its inverse

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


## Compute the inverse of the cache matrix returned by makeCacheMatrix()

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


