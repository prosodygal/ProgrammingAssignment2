## The following pair of functions caches the inverse of a matrix.

## The function below (makeCacheMatrix) creates a special "matrix" object 
## that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInverse<-function(solve) m <<-solve
  getInverse<-function()m
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The function below (cacheSolve) computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cachced data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setInverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
