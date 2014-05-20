## The two functions below can provide a enviroment which store a matrix and
## its invertion, which can be easily get without duplicative calculation.

## the makeCacheMatrix function creates a environment to store the matrix and 
## its invertion. What's more, it provide the get and set method to access to
## the variables.

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


## the cacheSolve function can solve the invertion of matrix x, and store it 
## to the cache where the invertion can be easily get without duplicative 
## calculation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
