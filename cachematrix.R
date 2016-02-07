## Put comments here that give an overall description of what your
## functions do
## The following code is for finding the inverse of matrix and caching
## it for faster computation

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  ## initalize the inverse matrix as null
  inv_x <- NULL
  ## function to set the original matrix
  set <- function(y) {
    ## set the original matrix to the input y
    x <<- y
    ## set the inverse to Null
    inv_x <<- NULL
  }
  ## funcion to get the original matrix
  get <- function() x
  ## function to set the inverse
  setInv <- function(inv) inv_x <<- inv
  ## function to get the inverse
  getInv <- function() inv_x
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## read the inverse matrix
  inv_x <- x$getInv()
  ## if inverse has already been calculated and cached, return the cached value
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  ## else, get the originla matrix and store in data
  data <- x$get()
  ## find the inverse
  inv_x <- solve(data,...)
  ## Set the inverse matrix for future caching
  x$setInv(inv_x)
  ## return the calculated inverse
  inv_x
  
}
