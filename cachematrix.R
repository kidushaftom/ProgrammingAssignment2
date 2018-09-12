##makeCacheMatrix: This function creates a special "matrix" 
##object that can cache its inverse.

makeCacheMatrix <- function(mt = matrix()) {
  I <- NULL
  set <- function(y) {
    mt <<- y
    I <<- NULL
  }
  get <- function() mt
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##cacheSolve: This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(mt, ...) {
  I <- mt$getinverse()  ## Return a matrix that is the inverse of 'x'

if(!is.null(I)) {
  message("getting cached data.")
  return(I)
}
data <- mt$get()
I <- solve(data)
mt$setinverse(I)
I
}


