## Put comments here that give an overall description of what your
## functions do

## creates a function called makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL ## s = inverse, null now because no inverse has been calculated yet
  set <- function(y) { ## creates function called set to replace the matrix
    x <<- y ## stores the new matrix 
    s <<- NULL 
  }
  get <- function() x ## creates a function that returns the matrix
  setsolve <- function(solve) { ## creates a function that stores an inverse
    s <<- solve
  }
  getsolve <- function() { ## returns the stored inverse
    s
  }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) ## returns all 4 functions
}

## creates a function called cacheSolve

cacheSolve <- function(x, ...) {
  s <- x$getsolve() ## asks is an inverse already stored
  if(!is.null(s)) {
    message("getting cached inverse")
    return(s) ## returns the stored inverse, no calculation
  }
  data <- x$get() ## if no inverse, retrieves matrix
  s <- solve(data, ...) ## calculates inverse 
  x$setsolve(s) ## stores inverse
  s ## returns inverse 
}
