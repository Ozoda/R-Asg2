makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## set default value 
  set <- function(y) {  ## initial function for caching  
    x <<- y
    m <<- NULL
  }
  get <- function() x ## get the input matrix 
  setinverse <- function(inverse) m <<- inverse ## change the value of cache after an inverse is made
  getinverse <- function() m ## obtain the inverse of cached matrix 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## input the inverse of the matrix if it exists 
  if(!is.null(m)) {   ## test if the inverse exists 
    message("getting cached data") ## if inverse exists, print the message ... 
    return(m) ## ... and fit in the cached results 
  }  ## if the inverse of the matrix has not yet been calculated, the followings will be run
  data <- x$get() ## input 
  m <- solve(data, ...)  ## calculate the inverse 
  x$setinverse(m) ## cache the inverse  
  m ## print the result of the inverse
}
