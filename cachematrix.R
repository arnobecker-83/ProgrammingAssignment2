## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
## The makeCacheMatrix is used for inputs to pass to the cacheSolve Function. 
## The values can be looked up in the cache rather than recomputed. 
makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
## Set the input value
  set <- function(y) {
## The <<- operator is used to assign a value to an object in an environment that is different from the current environment.
    x <<- y 
    INV <<- NULL
  }
## Get the input value
  get <- function() x
## Set the inverse value
  setInverse <- function(INVerse) 
    INV <<- INVerse
## Set the inverse value
  getInverse <- function() 
    INV
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
## Get the inverse value from the makeCacheMatrix function
    INV <- x$getInverse()
## If the inverse value is NOT NULL, then message: "getting cached data" is displayed with the return calculated value
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
## If the inverse value is NULL, then get the value to calculate with the solve function
  matrix_data <- x$get()
  INV <- solve(matrix_data, ...)
  ## The calculated inverse value is set and retrieved
  x$setInverse(INV)
  INV
}
