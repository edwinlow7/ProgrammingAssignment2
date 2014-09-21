## a pair of functions that cache the inverse of a matrix. First function
##creates a vector that contains 4 function to set/get a matrix and set/get
##its inverse matrix
##second function check if matric is set or changed. if not it calculates
##inverse and cache it.

## This function creates a special "matrix" object that can cache its inverse  

makeCacheMatrix <- function(x = matrix()) {
  
  #Cache Matrix Value
  Cache_Matrix<-NULL
  set <- function(y) {
    x <<- y
    Cache_Matrix <<- NULL
  }
  
  #Get Matrix value
  get <- function()x
  
  #Set inverse matrix
  setInverse<-function(InvMatrix)
    Cache_Matrix<<-InvMatrix
    
  
  #Get inverse matrix
  getInverse<-function()Cache_Matrix
  
  #create list
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
  
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  ##Get cache inverse matrix
  Inv<-x$getInverse()
  
  if(!is.null(Inv)) {
    message("Getting cached data")
    return(Inv)
  }
  data<-x$get()
  newInv<-solve(data)
  x$setInverse(newInv)
  newInv
}