## makeCacheMatrix provides the shell for the cached matrix 'cacheSolve' to calculate in. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set_it <- function(y) {
    x <<- y
    m <<- NULL
  }
  get_it <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set_it = set_it, get_it = get_it,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get_it()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Example 
## B <- matrix(c(2,1,5,3,2,1,2,3,9),nrow=3)
## solve(B)

## b <- makeCacheMatrix
## b$get_it()
## cacheSolve(b)
## b$getinverse()