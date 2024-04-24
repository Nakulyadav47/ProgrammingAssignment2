# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize a variable to store the inverse matrix
  inv <- NULL
  
  # Setter function to set the matrix
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL  # Reset the cached inverse when the matrix changes
  }
  
  # Getter function to retrieve the matrix
  get <- function() x
  
  # Function to compute the inverse of the matrix
  cacheInverse <- function() {
    # Check if the inverse is already cached
    if(!is.null(inv)) {
      message("Getting cached inverse")
      return(inv)
    }
    # If not cached, compute the inverse and cache it
    inv <<- solve(x)
    inv
  }
  
  # Return a list containing the setter, getter, and inverse calculation functions
  list(set = set, get = get, cacheInverse = cacheInverse)
}

# Function to compute the inverse of a matrix using cache if available
cacheSolve <- function(x, ...) {
  # Check if the cached inverse is available
  if(!is.null(x$inv)) {
    message("Getting cached inverse")
    return(x$inv)
  }
  # If not available, compute the inverse and cache it
  inv <- x$cacheInverse()
  inv
}
