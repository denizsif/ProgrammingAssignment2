##Functions makeCacheMatrix and cacheSolve can together compute and 
##cache the inverse of an input matrix, so next time the inverse of 
##that matrix is called, it is not computed but instead called from 
##the cache. This logic can be implemented to larger, repetitive 
##computations to speed up the process. 

##makeCacheMatrix creates a list of 4 functions that can be called by 
##cacheSolve function in order to test if the inverse of the input 
##matrix x is already in the cache and if not, to get the matrix x to 
##compute the inverse and deposit the calculated inverse matrix i in 
##the cache.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y;
    i <<- NULL;
  }
  get <- function() return(x);
  setinv <- function(inverse) i <<- inverse;
  getinv <- function() return(i);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

##cacheSolve function first checks if there is a computed inverse matrix 
##for x in the cache, and if there is returns it. If there is not a cached 
##value, it computes the inverse of matrix x and deposits it in the cache 
##through setinv function of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("Getting cached data...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  return(i)
}