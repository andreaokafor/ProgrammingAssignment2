## These functions cache the inverse of a matrix

## The first one create a special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      
      h <- NULL
      set <- function(y){
            x <<- y
            h <<- NULL
      }
      
      get <- function() x
      setinv <- function(inv) h <<- inv
      getinv <- function() h
      list(set = set, get = get, setinv = setinv, getinv = getinv)
      
}


## The second one computes the inverse ot the matrix returned
## by makeCacheMatrix. If the the inverse it's already 
## been calculated then the function should
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      
      h <- x$getinv()
      if (!is.null(h)){
            message("getting cache data")
            return(h)
      }
      data <- x$get()
      h <- solve(data, ...)
      x$setinv(h)
      h
}
