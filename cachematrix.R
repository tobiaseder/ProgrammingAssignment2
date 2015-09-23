## Task ofthe Function: Getting the Inverse of a Matrix.

## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly

## The function creates a 'matrix' object that can store its inverse in cache.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
       
}

## For tests, see section below 2nd function.

## ----------------------------------------------------------------

## 'cacheSolve' calculates the inverse of the matrix object of 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'

  inv <- x$getInverse()
  if (!is.null(inv)) {
  
    message("Getting Cached Data")
    return(inv)
    
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}

## ----------------------------------------------------------------

## ---------------------------------------------
## -TESTS: Apply settings in appropriate order -
## ---------------------------------------------

## My_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## My_Matrix$get()
## My_Matrix$getInverse()
## RESULT: NULL
## cacheSolve(My_Matrix)
## cacheSolve(My_Matrix)
## My_Matrix$getInverse()
