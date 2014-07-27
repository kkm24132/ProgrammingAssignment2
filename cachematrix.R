## R Programming - Program Assignment 2
## Author: Kamal Mishra
## A pair of functions that cache the inverse of a matrix
## Creates a special matrix object that can cache its inverse

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix" above.
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Message: getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


## Test Result - Sample Run:
## > x=rbind(c(1,-0.25),c(-0.25,1))
## > m=makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## Test Result - No cache in the First Run:
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Test Result -  Retrieving from cache from Second run:
## > cacheSolve(m)
## Message: getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

