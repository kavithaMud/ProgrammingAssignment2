## makeCacheMatrix enables caching a matrix for matrix inverse
## It exploses 4 methods to set, get the matrix AND to setinverse, getinverse of the matrix.
## This methods enables calling function to set the inverse into cache when it does not exist

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(minverse) m <<- minverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve function returns the inverse of a matrix.  
## The function first checks to see the matrix inverse is already available in the cache, 
## and if exists it returns the inverse matrix getting it from cache.
## if not is will compute the inverse and sets it into cache for future use.
## this function leverages methods explosed by makeCacheMatrix function to either set the newly computed matrix 
## inverse into cache, or to retrieve the inverse from cache if it already exists.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
