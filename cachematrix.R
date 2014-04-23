## This collection of functions is used to have matrices that caches their inverse
## in order to save a costly recalculation of the inverse everytime time.

## Create a special matrix that can cache its inverse.

makeCacheMatrix <- function(my_matrix = matrix()) {
  # Cached inverse is initially not calculated.
  cached_inverse <- NULL

  # Set the matrix data
  set <- function(y) {
    # Set internal representation to the new value y
    my_matrix <<- y 
    
    # Reset the cached inverse since the value has potentially changed  
    cached_inverse <<- NULL 
  }
  
  # Get the matrix data
  get <- function() {
    my_matrix
  }
  
  # Set the cached inverse
  setinverse <- function(inverse) {
    cached_inverse <<- inverse
  }
  
  # Get the cached inverse
  getinverse <- function() {
    cached_inverse
  }
  
  # Return the special matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Retrieve the inverse of a matrix. Will use a cache if it has already
## been calculated and the matrix has not changed since.

cacheSolve <- function(x, ...) {
  # Get the cached inverse
  inverse <- x$getinverse()
  
  # Return it if it has been calculated
  if (!is.null(inverse)) {
    return(inverse)
  }

  # Calculate the inverse from the matrix
  data <- x$get()
  inverse <- solve(data, ...)
  
  # Set the cached inverse for later retrievals
  x$setinverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}
