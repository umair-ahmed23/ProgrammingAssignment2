## my aim in this experiment is to write a pair of functions, namely,
## makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
## makeCacheMatrix is a function which creates a special "matrix" object that can
## cache its inverse for the input (which is an invertible square matrix)
## library mass is used to calculate inverse for non squared as well square matrix
library(mass)
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL        #initializing inv as NULL
  set <- function(y){
    x <<- y
    J <<- NULL      
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##cacheSolve is a function which computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache


cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){             #checking whether J is null
    message("getting cached data")
    return(j)                  #return inverse value
  }
  data <- x$get()
  j <- solve(data,...)          #calculates inverse value
  x$setInverse(j)
  j
}

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## lets get graded
## awaiting result