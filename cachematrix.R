## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                       ## keeping inv val as null
    set <- function(y) {            
            x <<- y                 ##assigning y to x matrix
            inv <<- NULL            
        }
      get <- function() x           ## defining and returning funtion to return matrix
      
        setinverse <- function(inverse) inv <<- inverse  ##assigning inverse to our created var
        getinverse <- function() inv                    ##returning inv of matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
