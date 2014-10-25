## Wraps a matrix into a 'class' that has the posibility to set the inverse

makeCacheMatrix <- function(x = matrix()) {
  # the inverse of X
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


## Solves a cache-able matrix (@see makeCacheMatrix)
## 1. if the inverse is cached return it
## 2. if not, solve it and store it in cache

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

