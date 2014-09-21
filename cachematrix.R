## Performing caching to matrix inversion to prevent costly computation
## and repeated computation

## The following function creates a matrix object that can cache an inverse.
## It takes in argument matrix x. It returns a matrix object with the 
## following functions: $set, $get, $setSolve and $getSolve. 
## $set - stores a new matrix with cached matrix set to NULL
## $get - retrieves the stored matrix
## $setSolve - stores the inversed matrix
## $getSolve - retrieves the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setSolve <- function(inverse) m <<- inverse
  getSolve <- function() m
  
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## Performs the inversion of the matrix object returned by makeCacheMatrix()
##The function will retrieve a cached inversed matrix in the object. If 
## the object doesn't exist, the function will compute the inverse matrix,
##stores it into the object and return the inversed matrix. 

## The function takes in the argument x which is a list object. It returns
## the inverse of the matrix in x. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
  
}

