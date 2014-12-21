## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            
            set <- function(y) {
                x <<- y
                inv <<- NULL
            }
            
            get <- function() {
                x
            }
            
            setinv <- function(i) {
                inv <<- i
            }
            
            getinv <- function() {
                inv
            }
            
            list(set = set, 
                 get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
          # get the cached inverse
          inv <- x$getinv()
  
          if(!is.null(inv)) {
          # if the inverse if actually cached, just return it
            message("getting cached inverse")
            return(inv)
          }
  
          # otherwise, calculate the inverse and cache it
          matr <- x$get()
          inv <- solve(matr, ...)
          x$setinv(inv)
  
          return(inv)
}
