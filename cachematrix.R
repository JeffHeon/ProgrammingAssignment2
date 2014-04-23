## This collection of functions is used to have matrices that caches their inverse
## in order to save a costly recalculation of the inverse everytime time.

## Creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(my_matrix = matrix()) {
  cached_inverse <- NULL
  
  set <- function(y) {
    # Set internal representation to the new value y
    my_matrix <<- y 
    
    # Reset the cached inverse since the value has potentially changed  
    cached_inverse <<- NULL 
  }
  
  get <- function() {
    my_matrix
  }
  
  setinverse <- function(inverse) {
    cached_inverse <<- inverse
  }
  
  getinverse <- function() {
    cached_inverse
  }
  
  # Return the special matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Retrieves the inverse of a matrix. Will use a cache it it has already
## been calculated and the matrix has not changed since.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if (!is.null(inverse)) {
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}
