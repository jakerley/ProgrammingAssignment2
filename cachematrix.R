## These 2 functions (makeCacheMatrix and cacheSolve) create a special "matrix" which exposes functions to get/set the
## cached matrix and inverse of the matrix as well as finding the inverse and setting it.
## 

## makeCacheMatrix creates a special "matrix", which is really a list containing functions to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      matrix <<- y
      inverse <- NULL
    }
    get <- function() matrix
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the setInverse function.
## The passed matrix is assumed to be invertible.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null( inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse = solve( matrix)
  x$setInverse( inverse)
  inverse
}