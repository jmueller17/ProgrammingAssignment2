## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  #for a new object, the inverse does not exist
  inverse <- NULL
  
  #returns the actual matrix
  getM <- function() {
    x
  }
  
  #if new matrix is set, reset inverse
  setM <- function(m=matrix()){
    x <<- m
    inverse <<- NULL
  }
  
  #stores the inverse matrix
  setIM <- function(im) {
    inverse <<- im
  }
  
  #returns the stored inverse or null if empty
  getIM <- function(){
    inverse
  }
  
  list(setInverseMatrix=setIM, getInverseMatrix=getIM, getMatrix=getM, setMatrix = setM)
}


## This function computes the inverse of the special "matrix" returned by 
#  makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
#  has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #check if inverse exists already
  inverse <- x$getInverseMatrix()
  if (!is.null(inverse)){
    message("getting cached inverse matrix")
    return(inverse)
  }
  
  #if inverse does not exist, get the matrix
  matrix <- x$getMatrix()
  
  # calc the inverse
  im <- solve(matrix)
  
  #store it to cache
  x$setInverseMatrix(im)
  
  #return it 
  im
}



