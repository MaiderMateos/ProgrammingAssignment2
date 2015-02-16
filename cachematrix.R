
## The functions below cache the inverse of a matrix.
# If the inverse of a matrix is calculated using the functions defined
# inside makeCacheMatrix and cacheSolve is called afterwards,
# then the inverse is not computed again but it is retrieved from the cache.
# This implies time saving.

# This function constructs a list of four functions: set, get, 
# setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   
   # set the value of the matrix
   set <- function(y){
      x <<- y
      m <<- NULL
   }
   
   # retrieve the value of x (the matrix)
   get <- function() x
   
   # set the value of the inverse matrix
   setinverse <- function(inverse) m <<- inverse
   
   # retrieve the inverse matrix
   getinverse <- function() m
   
   # returns the list of functions
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function retrieve the inverse of a matrix from cache if it has
# already been computed. Otherwise, the inverse is calculated.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   
   # chekc if the inverse matrix has already been computed.
   # If so, it is returned
   if(!is.null(m)){
      message("getting cache matrix")
      return(m)
   }
   
   # if the inverse matrix has not been obtained yet, compute and return it
   data <- x$get()
   m <- solve(data,...)
   x$setinverse(m)
   m
}
