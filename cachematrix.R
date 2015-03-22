## The first function creates a special matrix that stores the inverse of a matrix in 
##it cache; values fr ojects are assigned to global environments by the <<- operator.
## The second function calculates reports the inverse of the matrix created by
##the first function, but first checks to see if it's stored in the cache.
## If it is, then it just reports from the cache instead of calculating it.

## Create a special matrix that can store it's inverse in cache.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## calculates the inverse of the matrix created by the first function
##and reports from the cache instead of calculating it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
