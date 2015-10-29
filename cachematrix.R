## Matrix inversion is very mundane work and its iteration can cost us much. 
## Hence we can cache the inverse of a matrix rather than compute it repeatedly.
# The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix function perform below tasks.
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
inver<- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function returns the inverse of the matrix. It first checks if
## The inverse has already been computed and stored in cache. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via 
# setinverse function. 
cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached inverse data")
    return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinverse(inver)
    inver
}
