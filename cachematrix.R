## The inverse of a matrix is hard to be computed.
## The following two functions can be used to create a matrix and cache the inverse.

## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse) inverse<<-inv
  getInverse<-function() inv
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getInverse()
    if(!is.null(inv)){
      message("cached data")
      return(inv)
    }
    mat<-x$get()
    inv<-solve(mat,...)
    x$setInverse(inv)
    inv
}

matrix <- matrix(1:4, 2)
cachedMat <- makeCacheMatrix(matrix)
result <- cacheSolve(cachedMat)
print(result)

## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5