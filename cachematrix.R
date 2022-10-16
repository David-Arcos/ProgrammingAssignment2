makeCacheMatrix <- function( mat = matrix() ) {
  
    inv <- NULL ## Initialize the inverse property
  
  ## Set function for a matrix
  set <- function( matrix ) {
    mat <<- matrix # assigns the input "matrix" of the "set" function, to the "mat" object in the parent environment
    inv <<- NULL # assigns the value of NULL to the "inv" object in the parent environment
               # This line clears any previous value of "inv"   
  }
  
  ## Get the matrix and then return it
  get <- function() {
    mat
  }
  
  ## Set the inverse of the matrix and assign it to the "inv" object in the parent environment
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the inverse of the matrix and return it
  getInverse <- function() {
    inv
  }
  
  ## Create a list with all the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getInverse()
  
  ## Just return the inverse if it's already set
  if( !is.null(mat) ) {
    message("getting cached data")
    return(mat)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using solve()
  mat <- solve(data, ...)
  
  ## Set the inverse to the object
  x$setInverse(mat)
  
  ## Return the matrix
  mat
}

a <- matrix(c(1,2,3,4),2,2)
b <- makeCacheMatrix(a)
cacheSolve(b) #inverse returned after computation
b$setInverse()
