## Create a special matrix that contains the functions to do the following:
# Set the matrix
# Get the matrix
# Set the inverse of the matrix
# Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set <- function(y) {
    x <<- y 
    i <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function () i 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## If an inverse has been calculated and the matrix is unchanged, retrieve the inverse from cache. Otherwise, calculate new inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) { 
     message("getting cached data")
     return(i) 
  } 
  data <- x$get() 
  i <- solve(data, ...) 
  x$setinverse(i)
}   
