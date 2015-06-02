## Creates cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
  x_ <- NULL
  set <- function(y){
    x <<- y
    x_ <<- NULL  
  }
  get <- function() x
  setInversion <- function(i) x_ <<- i
  getInversion <- function() x_
  list(set = set,get = get,
       setInversion = setInversion,
       getInversion = getInversion)
}


## Inverts cachable matrix and caches its invertion

cacheSolve <- function(x, ...) {
  x_ <- x$getInversion()
  if (!is.null(x_)) {   
    return (x_)
  }
  x$setInversion(solve(x$get(), ...))
  x$getInversion()
}
