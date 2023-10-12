## Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mat <- x
  inv <- NULL
  
  setMatrix <- function(matrix) {
    mat <<- matrix
    inv <<- NULL # Reset the inverse cache
  }
  
  getMatrix <- function() {
    mat
  }
  
  getInverse <- function() {
    if(!is.null(inv)) {
      message("Getting cached inverse")
      return(inv)
    } else {
      message("Computing inverse")
      inv <- solve(mat)
      return(inv)
    }
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse)
}

## Computes the inverse of the special matrix, using caching.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  } else {
    message("Computing inverse")
    inv <- solve(x$getMatrix())
    x$setInverse(inv)
    return(inv)
  }
}

# Let's do some examples to try the functions
# Create a special matrix
myMatrix <- makeCacheMatrix(matrix(c(4, 3, 2, 1), nrow = 2))

# Get the matrix
matrix <- myMatrix$getMatrix()
print("Original Matrix:")
print(matrix)

# Compute the inverse (and cache it)
inverse <- cacheSolve(myMatrix)
print("Inverse:")
print(inverse)

# Retrieve the cached inverse
cached_inverse <- cacheSolve(myMatrix)
print("Cached Inverse:")
print(cached_inverse)