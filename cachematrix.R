## These two functions together will allow you to save memory be creating a cache of 
## matrices which have been inverted, therefore removing the necessity of computing the
##inversion of already computed matrices

## Creates a special list contained of functions about the submitted matrix
##getinverse stores the inverse of the matrix if it has been inverted. If it has not,
##setinverse can be called to set the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Returns the inverse of the function. If the inverse has already been calculated, it simply
##cals x$getinverse to retrieve it. If not, it calculates the inverse by calling setInverse.

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

