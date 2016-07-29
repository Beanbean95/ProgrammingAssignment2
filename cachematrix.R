## These two functions will enable the inversion of a square matrix, and
##cache the result to the object: matrix. 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrix<-NULL
  set<-function(y){
    x <<- y
    matrix <<- NULL
  }
  get <-function() x
  setinverse <-function(inverse) matrix <<- inverse
  getinverse <-function() matrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
  
}


## This function computes the special inverse of a matrix enabled by
## the above function, and should retrieve the inverse from cache
## if the inverse already exists. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix<-x$getinverse()
  if(!is.null(matrix)){
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data,...)
  x$setinverse(matrix)
  matrix
}
