## These functions implement matrix class, which has attributes and methods.

##This function creates matrix, which has cacheable state information,
## for instance inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns inverse of matrix. It calculates the inverse,
## if not calculated before

cacheSolve = function(x, ...) {
  
  i <- x$getinverse()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data)
  
  x$setinverse(i)
  i
}
