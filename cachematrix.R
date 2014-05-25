## This script contains two main R functions. 
## The first, makeCacheMatrix is used to create a matrix object where the inverse can be cached.
## The second function, CacheSolve uses the cached inverse if it is available or calculates the 
## inverse if it is not.
##
## Taking the inverse of large matrix can be computationally intense and these functions means it only has to 
## be calculated once
##
## Note: The input matrix is assumed to be invertible.


## The makeChaceMatrix function creates a special "matrix" object that can cache its inverse.
## The matrix object is actually a list which contains a function to:
## 1) set the matrix
## 2) get the matrix
## 3) set the inverse matrix
## 4) get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The cacheSolve function checks if an inverse of x has already been calculated and uses  has.
## the cached value if it has. If not the inverse if calculed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}

