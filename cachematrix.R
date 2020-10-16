## makeCacheMatrix creates an object where the matrix and its inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x<<-y
            inv<<-NULL
      }
      get<-function() x
      setinv <- function(solve) inv<<-solve
      getinv<-function() inv
      list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve verifies if the inverse of the matrix is found in the cache, 
## if it finds a value it returns it, otherwise it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){
            message("Getting cached data")
            return(inv)
      }
      data <- x$get()
      inv<-solve(data,...)
      x$setinv(inv)
      inv
}
