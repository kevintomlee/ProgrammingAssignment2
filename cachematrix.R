## Put comments here that give an overall description of what your
## functions do
## function makeCacheMatrix() creates a list of functions
## that stored the original matrix and the invese matrix if it has been calculated
## function cacheSolve() calculates the inverse matrix 

## Write a short comment describing this function
## this function returns a list which:
## set the value of the original matrix
## get the value of the original matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inversem <- NULL
  
  set <- function(y) {
    x <<- y
    inversem <<- NULL
  }
  
  get <- function() x
  
  calinverse <- function(inverse) inversem <<- inverse
  
  getinverse <- function() inversem
  
  list(set = set, get = get,
       calinverse = calinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## calculate the inverse matrix and cache it
## and it assumes that matrix x is invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversem <- x$getinverse()
  if (!is.null(inversem)) {
    message("getting cached data")
    return(inversem)
  }
  data <- x$get()
  inverse <- solve(data)
  inversem <- x$calinverse(inverse)
  inversem
}
