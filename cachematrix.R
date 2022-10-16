library(MASS)
?ginv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## Initialize the x matrix and the inverse 
  
  ## Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Get the matrix and then return it
  get <- function() x
  
  ## Set the inverse of the matrix and assign it to the "inv" object in the parent environment
  setinverse <- function(inverse) inv <<- inverse
  
  ## Get the inverse of the matrix and return it
  getinverse <- function(){
    inver <- ginv(x)
    inver%*%x
  }
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using solve function
  inv <- solve(data, ...)
  
  ## Set the inverse to the object and return it
  x$setinverse(inv)
  
  inv
}


a <- makeCacheMatrix(matrix(1:8,2,4))
a$get()
a$getinverse()
