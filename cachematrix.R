## This file contains two functions:
##
## makeCacheMatrix takes a matrix as an argument
##  it creates a list made up of functions to store and
##  retrieve the matrix and its inverse.
##
## cacheSolve computes the inverse of a matrix.  If the
##  matrix and its inverse have been previously stored
##  cacheSolve uses the functions of makeCacheMatrix to 
##  retrieve and return the stored values.
##  If this is a new matrix, cacheSolve computes the
##  inverse and calls the functions of makeCacheMatrix 
##  to store the values
## 

## makeCacheMatrix creates a structure to store the inverse
## of matrix x
##  function getMatrix retrieves the stored inverse
##  function setMatrix computes and stores the inverse
##  function set stores matrix x and sets the inverse to null
##     this initializes the values
##  function get retieves the stored matrix x

makeCacheMatrix <- function(x = matrix()) {

  xInv<-NULL  ## initialize
  set<-function(y) {
    x<<-y
    xInv<<-NULL
  }
  get<-function() x
  setMatrix <-function(solve) xInv<<-solve
  getMatrix <-function() xInv
  list(set = set,get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## cacheSolve checks to see if a matrix has been stored. If so
## return the stored inverse.  Otherwise, compute the inverse,
#  store the inverse and return the new matrix.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  xInv<-x$getMatrix()  ## retrive matrix stored by makeCacheMatrix
  
##  if xInv is not null, inverse has been cached.  Just 
##   return it

  if (!is.null(xInv)) {
    message("getting cached data")
    return(xInv) 
  }
##  Inverse must be computed.  use get to get the original 
##  matrix, solve to compute the inverse, then store the inverse
##  and display it.

  else {
     data<-x$get()
     xInv<-solve(data)
     x$setMatrix(xInv)
     xInv
  }
    
}
