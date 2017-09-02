
## These functions calculate the inverse of a matrix and cache the 
## value. Because of lexical scoping in R, the entire makeCacheMatrix()
## environment as defined at the design stage stays in memory, and can
## be assigned to an S3 object that retains a complete copy of this environment.

## makeCacheMatrix() first initialises two objects: the formal argument 'x'(a matrix)
## and 'inverse', which is used later on in the code. Four functions are then defined,
## accessing values in the parent environment by lexical scoping.  Then set()
## is defined, allowing new values to be assigned to the x argument and ensuring
## a cached value of invese is cleared. get() simply retrieves x from the parent 
## environment. setinverse() assigns a new value to inverse. getinverse() 
## retrieves the value of inverse. Finally makeCacheMatrix() assigns each of these as a 
## named element within a list, allowing them to be accessed by the $ extract operator. 
## The list is then returned to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  
  get<-function()x
  setinverse<-function(solved_inverse) inverse <<-solved_inverse
  getinverse<-function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}
## CacheInverse() initialises a single argument'x', an object of type 'makeCacheMatrix()' ,
## and allows further arguments to be called with the ellipsis. First it retrieves 
## the value of inverse from the argument x, and determines if there is a cached inverse 
## value. If so, this value is printed. If not, the value of the inverse object is set to the 
## inverse of the original makeCacheMatrix() argument and prints this value.

CacheInverse<-function(x,...){
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message("getting inverse from cache")
    return(inverse)
  }
  
  inverse<-solve(x$get())
  x$setinverse(inverse)
  inverse
}







