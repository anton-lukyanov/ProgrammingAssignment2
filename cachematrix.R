## cachematrix.R contains two functions
## First one initialize matrix and functions
## Second one return inversed matrix


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


## This function calculate invers of input matrix x, if it called for
## the first time
## If inverse already calculated and there a no changes in matrix x
## it gets inverse matrix from the cash


cacheSolve<- function(x) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  message("inverse matrix") ## to check when matrix inverse is calculating
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
