## This function is used for caching the inverse of a matrix
## in order to save computation time and reduce expenses
## Four functions are created - set, get, setInv, getInv
## <<- assignment operator is used to protect internal 
## variable from outside environment.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xinv<-NULL
  set<-function(y) {
    x <<- y
    xinv <<- NULL
  }
  get<-function() x
  setInv<- function(xinv) xinv
  getInv<- function() xinv
  list(set=set,get=get, setInv=setInv, getInv=getInv)
}


## The data cache is executed from this environment

cacheSolve <- function(x) {
  xinv <- x$getInv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data)
  x$setInv(xinv)
  xinv  ## Return a matrix that is the inverse of 'x'
}

