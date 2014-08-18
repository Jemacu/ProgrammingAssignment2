## Functions to calculate the inverse of a matrix and cache the inverse
## or, to retrieve the inverse, if it has already been calculated and saved.

## Function to create a spectial 'matrix' object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <-function (y){
          x<<-y
          m<<-NULL
     }
     get <- function()x
     setinverse <- function(solve) m<<-solve
     getinverse <- function() m
     list(set=set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Function to compute the inverse of a special 'matrix' object returned by the
## makecacheMatrix function or retrieve the inverse if it has already been calculated.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <-x$getinverse()
     if(!is.null(m)){
          message("getting cached data")
          return (m)
     }
     data <-x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
     
}
