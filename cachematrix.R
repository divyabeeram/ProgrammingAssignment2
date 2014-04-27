## The two functions here cache inverse of the matrix and return the value, whenever it is 
##called. This avoids computing inverse each time the function is called.

## makeCacheMatrix computes a special vector which is a list of functions. This calculates
## has setter and getter functions for matrix and inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
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


## This function checks if the inverse is already calculated and returns the value from cache.
## If it is not present, it computes the inverse and saves it into the variable "inv".

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'abcdfihfoeijv
}
