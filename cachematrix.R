## makeCacheMatrix create the special vector that saves any wished
## transformation of a matrix in cache, very similar to the example.

##Create cache vector for the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(minv) m <<- minv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cache version of the function Solve
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse of a matrix")
    return(m)
  }
  data <- x$get()
  #Pass extra parameters in "..." but be sure it will estimate the inverse
  m <- solve(data, diag(dim(mymat)[1]), ...)
  x$setinv(m)
  m
}
