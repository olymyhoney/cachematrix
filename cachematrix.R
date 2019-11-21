## The first function returns a list including elements like set, get and so on. Those elements
## just creat the foundation of running the second function, which will calculate the inverse
## of a matrix. Except for simply calculating the inverse of a matrix, it includes storing the 
## result into a cache, which will reduce the computation process.

## makeCacheMatrix function just provides some basic functions for latter use

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y = matrix()) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function will check whether or not the same computation process appeared in order
## to avoid recomputing

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
