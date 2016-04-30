## Programming Assignment 2 : Lexical Scoping
## Matrix Inversion can be time consuming, to avoid the recomputation
## it is sensible to cache the result. Following 2 Functions are used
## to create the matrix and cache the inverse of the matrix

## makeCacheMatrix:
## This function takes a matrix vector as input, inverse of which needs to be cached
## This function creates a list which contains functions to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

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


## cacheSolve:
## This function checks if the inverse of the matrix created with 'makeCacheMatrix'
## is in the cache and skips the computation.
## Otherwise it calculates the inverse and and sets the value of the inverse in the 
## cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
