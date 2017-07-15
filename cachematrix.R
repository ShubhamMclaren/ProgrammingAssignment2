## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  dim1<-dim(x)
  MtrxRet<- matrix()   ##Return from function: 'MtrxRet'= Matrix Return
  dim(MtrxRet)<dim1    ##initializing the matrix vector, for equalling dimensions
  
  MtrxRet<-NULL
  set<-function(y){
    x<<-y
    MtrxRet<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) MtrxRet<<-inverse
  getinverse<-function() MtrxRet
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
    
  MtrxRet<-x$getinverse()
  if(is.null(MtrxRet)==FALSE){
    ##print("Getting cached")
    message("Getting cached data")
    return(MtrxRet)
  }
  data<-x$get()
  MtrxRet<-solve(data, ...)
  x$setinverse(MtrxRet)
  
  return(MtrxRet)
  
}
