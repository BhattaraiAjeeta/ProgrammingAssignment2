makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))
}
cacheSolve <- function(x,...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  m <- solve(x$get())
  x$setinverse(m)
  m
}