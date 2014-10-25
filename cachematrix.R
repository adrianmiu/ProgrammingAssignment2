## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    inverseX <<- inverse
  }
  
  getInverse <- function() {
    inverseX
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverseX <- x$getInverse()
  if(!is.null(inverseX)) {
    return(inverseX)
  }
  data <- x$get()
  
  inverseX <- solve(data)
  x$setInverse(inverseX)
  inverseX
}

