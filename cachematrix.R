## I tried to replicate the example almost exactly. Only 
## substituting the numeric values for matrices and mean function for solve.


makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

CacheSolve<-function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    message("Using cached result")
    return(inv)
  }
  data = x$get()
  inv = solve(data, ...)
  x$setinv(inv)
  
  return(inv)
}