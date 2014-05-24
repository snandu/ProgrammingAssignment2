## Put comments here that give an overall description of what your
## functions do
## These functions cache the matrix inverse of a given matrix so as to
## eliminate the need to recompute the inverse everytime it is needed
## which can be costly. They just return the cached valued instead of
## recomputing it when the base matrix hasn't changed.

## Write a short comment describing this function
## This function returns a vector of 4 access functions to a given matrix x
## 1. set : set the value of x (and nullify the cached inverse)
## 2. get : get the value of x
## 3. setinverse : cache the supplied inverse of x
## 4. getinverse : returned the cached inverse if set before or null otherwise
makeCacheMatrix <- function(x = matrix()) {
  # inv will hold the cached inverse
  inv <- NULL
  # the access functions describe above
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function will calculate the the inverse of a matrix.
## If it has already been calculated and cached before the cached value
## is returned.
## Otherwise the inverse is calculated and returned (and cached)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  # if inverse already calculated and cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Else calculate the inverse, cache it and return it
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
