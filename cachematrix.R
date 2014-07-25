## In conjunction, these functions return the inverse value of an
## instance of a matrix; if matrix passed is unchanged, functions will
## fetch cached inverse value and return it.

## This function when called instantiates a matrix then stores its
## value and inverse inside of new cached variables. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function "solves" the instance of the matrix passed to return
## its inverse, or return inverse if fetched from makeCacheMatrix
## above.

cacheSolve <- function(x, ...) {
  i <- x$solve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
