##
# Author: Anthony Smith
# JHU R Programming Week 3 Programming Assignment 2
# 6 June 2020

# write a pair of functions that
# cache the inverse of a matrix.
# 
# Write the following functions:
#   
#   1.  `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



# 2.  `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
# 
# Computing the inverse of a square matrix can be done with the `solve`
# function in R. For example, if `X` is a square invertible matrix, then
# `solve(X)` returns its inverse.

# Git Issues


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
      
}


# Let's test it out

M <- matrix(c(4,3,2,1),2,2)
Mcache <- makeCacheMatrix(M)
cacheSolve(Mcache)
cacheSolve(Mcache)
