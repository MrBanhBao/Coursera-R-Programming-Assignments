## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            #operator to assign a value to an object in an environment that is different than the current environment
            x <<- y 
            i <<- NULL
      }
      
      get <- function() x
      
      setInverse <- function(inverse) i <<- inverse
      
      getInverse <- function() i
      
      #map functions into a list
      list(set=set, get=get, 
           setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getInverse()
      if(!is.null(i)) {
            message("Getting cached Data.")
            return(i)
      }
      
      data <- x$get()
      i <- solve(data)
      x$setInverse(i)
      i
}
