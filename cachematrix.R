## makeCacheMatrix produces a matrix that allows you to set the contents of your matrix, get the contents of your matrix, 
##set the inverse to the inverse of your matrix, and get the inverse of your matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix. If the inverse of that particular matrix has already been calculated, the inverse will be cached, accessed and re-printed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
