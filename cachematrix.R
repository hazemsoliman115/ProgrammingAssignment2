## Put comments here that give an overall description of what your
## functions do
## The following code is for finding the inverse of matrix and caching
## it for faster computation

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setInv <- function(inv) inv_x <<- inv
  getInv <- function() inv_x
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getInv()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data,...)
  x$setInv(inv_x)
  inv_x
  
}
