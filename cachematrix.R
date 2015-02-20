## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly
##  I am going to  write a pair of functions that cache the inverse of a matrix.

## 1.  `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

## The first function, `makeCacheMatrix` creates a special "Matrix", which is really a list containing a function to

#1.  set the value of the Matrix
#2.  get the value of the Matrix
#3.  set the value of the Inverse of a Matrix
#4.  get the value of the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix(x)) {
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function()x
  setsolve<-function(solve) s<<-solve 
  getsolve<-function()s
  list(set=set,get=get,setmean=setmean,getmean=getmean)
}


## The following function calculates the inverse of the special "matrix"
##created with the above function. However, it first checks to see if the
##mean has already been calculated. If so, it `get`s the inverse of the matrix from the
##cache and skips the computation. Otherwise, it calculates the inverse of the matrix
##and sets the inverse of the matrix in the cache via the `setmean`function.


cacheSolve <- function(x, ...) {
        s<-x$getsolve()
      if(!is.null(s)){
        message("getting cached data")
        return(s)
      }
      data<-x$get()
      s<-solve(data,...)
      x$setsolve(s)
      s
}
