# The follwoing function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y                 # use <<- to assign a value to an object in an environment 
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()        #inverse of the original matrix input to makeCacheMatrix()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setinverse(inver)
  inver
}
