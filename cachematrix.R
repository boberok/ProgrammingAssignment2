## These two functions work together to save computation
## time.They allow a calculated value to be cached and retrieved
## if necessary rather than continually recalculating it.

## This function creates a list of functions that 
## will be called in the second function therefore
## it must be passed as an argument to that function.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calls functions defined in the
## first function. It calculates the inverse of 
## a matrix and then caches it so if it is asked
## to calculate an inverse that it has previously
## calculated it will simply retrieve it instead 
## of recalculating it.

cacheSolve <- function(x, ...) {
	m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}