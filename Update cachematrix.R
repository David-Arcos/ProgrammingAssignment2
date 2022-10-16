library(MASS)
?ginv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function(){
    inver <- ginv(x)
    inver%*%x
  } 
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  i
}


a <- makeCacheMatrix(matrix(1:8,2,4))
a$get()
a$getinverse()
