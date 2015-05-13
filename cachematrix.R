## This is my attempt at a pair of functions that
## cache the inverse of a matrix.

## Creates the special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)

}


## This function computes the inverse of the special meatrix
## returned by makeCacheMatrix. If the inverse has been calculated
## and the matrix has not been changed, the cachesolve() functon
## should get the inverse from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
