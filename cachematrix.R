## This porgram comprises of two functions: makeCacheMatrix and cacheSolve. These two functions make sure 
## that the inverse of the matrix is stored in cache which helps mitigates recomputing of the inverse.

## makeCachematrix generates a special matrix object list that can cache its inverse. getmat gets the data,
## setmat sets the inverse. getinv fetchesthe inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes in the output of makeCache matrix as input. Checks for inv. If inverse is available, then
## outputs it, else gets the data and then computes the inverse using "solve" and caches the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  inv
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmat()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
