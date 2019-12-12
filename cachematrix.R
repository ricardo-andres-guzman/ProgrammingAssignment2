##
##  makeCacheMatrix(x) creates a special "matrix", which is really
##  a list containing a function to
##
##  1. Set the value of the matrix: set(x)
##  2. Get the value of the matrix: get()
##  3. Set the value of the inverse: setinv(y)
##  4. Get the value of the inverse: getinv()

makeCacheMatrix <- function(x = matrix()) {
  
  inv_x <- NULL
  
  set <- function(new_x) {
    
    x     <<- new_x
    inv_x <<- NULL
  }
  
  get    <- function()  x
  setinv <- function(y) inv_x <<- y
  getinv <- function()  inv_x
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve(x) calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via the setinv(y) 
## function.

cacheSolve <- function(x, ...) {
  
  inv_x <- x$getinv()
  
  if(!is.null(inv_x)) {
    
    message("getting cached data")
    return(inv_x)
  }
  
  inv_x <- solve(x$get(), ...)
  x$setinv(inv_x)
  inv_x
}
