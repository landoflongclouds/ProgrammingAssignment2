## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse, also raise message if an un-invertiable matrix is supplied. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    if((class(try(solve(y),silent=T))=="matrix") ){
      x <<- y
      m <<- NULL
    }else{
      message("Please supply invertible matrix!")
    }
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function checks if cached inverse result exists, returns cached if exists else calcualte the inverse

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
