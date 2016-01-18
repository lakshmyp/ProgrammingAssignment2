## This code contains 2 outer functions that can create a matrix object, solve for 
## its inverse, and cache the inverse matrix. If the cache still contains the inverse,
## a repeated call to calculate the inverse via cacheSolve will pull the inverted 
## matrix from the cache instead of re-calculating it. 

## makeCacheMatrix has a list of 4 functions that can create a matrix
## return the matrix object, set it to a matrix object, and caches its inverse. You can
## also reset the inverse here

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(sol) m <<- sol
  getinv <- function() m
  

  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks the cache if the inverted matrix is already calculated and stored, 
## if not, it calculates the inverted matrix and saves it in cache and returns 
## the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
