## Caching the inverse of a matrix

## Creating a special square invertible matrix and list of function
## which sets the value of matix, gets the value of matrix, sets the 
## value of inverse, and gets the value of inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve()
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##  Calculating the inverse of the matrix that is given by makeCacheMatrix function 
##- if the inverse already exists, the cachesolve will give the inverse from the cache.
##- if not, it calculates inverse and caches it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(matrix, ...)
  x$setinv(m)
  m
}
