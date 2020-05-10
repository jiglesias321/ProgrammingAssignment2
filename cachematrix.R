## Put comments here that give an overall description of what your
## functions do

## We generate a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Takes the result of the previous function stored in makeCacheMatrix and
## computes the inverse of it. If the inverse has already been calculated,
## then it should retrieved the inversed from the cache 

cacheSolve <- function(x, ...) {
  a <- x$getInverse()
  if(!is.null(a)){
    message("getting data in cache")
    return(a)
  }
  matr <- x$get()
  a <- solve(matr,...)
  x$setInverse(a)
  a
}
