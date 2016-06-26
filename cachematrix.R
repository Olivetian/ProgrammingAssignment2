## Write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)i<<-inverse
  getinverse<-function()i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the sepcial "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated(and the matrix has not changed), then the 
## cachesovle should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data.")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setinverse(i)
  i
}

##> x<-matrix(c(2,1/2,-1/2,2),2,2)
##> m<-makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]  2.0 -0.5
##[2,]  0.5  2.0
##> cacheSolve(m)
##[,1]      [,2]
##[1,]  0.4705882 0.1176471
##[2,] -0.1176471 0.4705882
##> cacheSolve(m)
##getting cached data.
##[,1]      [,2]
##[1,]  0.4705882 0.1176471
##[2,] -0.1176471 0.4705882
##>
