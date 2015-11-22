## Create a function to find Inverse of a Matrix
## In the second call to function cache the inverse. 

## Find Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Cache the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("----------getting cached data----------------")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

x <- c(4,2)
y <- c(7,6)
m1<-cbind(x, y)

tmp1 <- makeCacheMatrix(m1)
cacheSolve(tmp1)
cacheSolve(tmp1)

