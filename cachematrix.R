## Caching the Inverse of a Matrix


## function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
  if(! is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  m <- solve(x$get())
  x$setmatrix(m)
  m
}
