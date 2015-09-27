## Functions for programming assignment 2 of the Coursera course "R Programming". 

## makeCacheMatrix creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the value of the matrix inverse to NULL
  matinv <- NULL
  ## Define a function that caches the value of the matrix and its inverse
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  ## Assign the value of the matrix to get
  get <- function() x
  setinverse <- function(solve) matinv <<- solve
  getinverse <- function() matinv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the matrix has not changed, 
## then cacheSolve retrieves the inverse from the cache.
## This function assumes that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
  ## Look for the matrix inverse
  matinv <- x$getinverse()
  ## If the inverse exists, return it
  if(!is.null(matinv)) {
    message("getting cached matrix inverse")
    return(matinv)
  }
  ## If the inverse does not exist, calculate and return it
  data <- x$get()
  matinv <- solve(data, ...)
  x$setinverse(matinv)
  matinv
}
