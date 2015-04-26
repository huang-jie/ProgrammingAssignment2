## makeCacheMatrix provides the shell for the cached matrix 'cacheSolve' to calculate in. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set_it <- function(y) {
    x <<- y
    m <<- NULL
  }
  get_it <- function() x ## get_it returns the original matrix that was included in the function parameters
  setinverse <- function(solve) m <<- solve ## setinverse is used by cacheSolve to perform the inversion
  getinverse <- function() m ## getinverse will return the inverted matrix after the call to cacheSolve
  ## list is the set of functions above that makeCacheMatrix can call
  list(set_it = set_it, get_it = get_it, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve uses the calls within makeCacheMatrix to set and cache the inverted matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## gets the inverted matrix if not already cached
  if(!is.null(m)) { ## loop that gets the cached matrix if cached, if not then gets original matrix and creates the inverted matrix
    message("getting cached data")
    return(m) ## returns the cached matrix
  }
  data <- x$get_it() ## gets the original matrix from makeCacheMatrix
  m <- solve(data, ...) ## calculates the inverted matrix
  x$setinverse(m) ## sets the cached inverse to m
  m ## prints the inverted matrix
}

## Example 
## B <- matrix(c(2,1,5,3,2,1,2,3,9),nrow=3)
## solve(B)

## b <- makeCacheMatrix
## b$get_it()
## cacheSolve(b)
## b$getinverse()