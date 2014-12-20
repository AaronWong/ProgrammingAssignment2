## These functions can cache the inverse of a 
## matrix rather than compute it repeatedly.

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
        x <<- y
        s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
      if(!is.null(s)) {                 
        message("getting cached data") 
        return(s)                       ## Retrieve the inverse 
                                        ## of 'x' from the cache.
      }                                
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s                                 ## Return a matrix that is 
                                        ## the inverse of 'x'
}

