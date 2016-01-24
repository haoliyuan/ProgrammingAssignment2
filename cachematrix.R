## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invCache = NULL
  set <- function(y) {
    x <<- y
    invCache <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) invCache <<- inv
  getinverse <- function() invCache
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# Calculate the inversed matrix and save it in the cache
# If the inversed matrix is already in the cache then return it directly.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Try find it in the cache first
  invCache <- x$getinverse()
  if(!is.null(invCache)) {
    message("Cached inverse: ")
    return(invCache)
  }
  
  # Get data
  data <- x$get()
  # Calculate the inversed matrix
  invCache <- solve(data)
  
  # Cache the inversed matrix
  x$setinverse(invCache)
  
  # Return the inversed matrix
  invCache
}
