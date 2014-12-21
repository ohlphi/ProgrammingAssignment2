## My comments on what this does. Basically, it is very similar to
## the example with the vector.


# We create a special "matrix" object that cache the inverse
makeCacheMatrix <- function(x = matrix()) {
            
            # The inverse is initially stored as NULL
            inv <- NULL
            
            # Set sets the value of the matrix
            set <- function(y) {
                x <<- y
                inv <<- NULL
            }
            
            # Get returns the value of the original matrix
            get <- function() {
                x
            }
            
            # Setinv sets the value of the inversed matrix,
            # which is called by cacheSolve() for the first 
            # cacheSolve()
            # It will store the value using the superassignment <<-
            setinv <- function(i) {
                inv <<- i
            }
            
            # Get returns the value of the inversed matrix from 
            # cacheSolve()
            getinv <- function() {
                inv
            }
            
            # The list is accessed each time MakeCacheMatrix() 
            # creates a new object.
            # This is a list of internal functions, so a calling
            # function knows how to access them.
            list(set = set, 
                 get = get,
                 setinv = setinv,
                 getinv = getinv)
}



## The following function calculates the inverse of the matrix,
## created with the function above.

cacheSolve <- function(x, ...) {
          # First get the inverse to check if it has already been
          # cached
          inv <- x$getinv()
  
          # If the inverse is already cached, just return
          # with a message that it is 'getting cached inverse'
          if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
          }
  
          # If not, calculate the inverse and cache it
          matr <- x$get()
          inv <- solve(matr, ...)
          x$setinv(inv)
  
          return(inv)
}

## Example: 

# matr <- makeCacheMatrix(matrix(1:4, 2, 2))
#> cacheSolve(matr)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#> cacheSolve(matr)
#getting cached inverse
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5