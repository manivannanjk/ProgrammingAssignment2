## The makeCacheMatrix and cacheSolve functions illustrate the use of
## cache in executing large and complex data operations.
## They use the inverse of a matrix as an example.

## The makeCacheMatrix function sets the value of the matrix and 
## defines the inverse function.It does "setting and getting".
## Here, m is the matrix and inv is the inverse.
## The function returns the list to the parent environment.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
    get <- function() m
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## The cacheSolve function executes the inverse of a matrix
## if there is a new data set. Otherwise, it publishes a message
## "Getting cached data" and retrieves the stored data from cache.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  inv <- m$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinv(inv)
  inv
  
}
