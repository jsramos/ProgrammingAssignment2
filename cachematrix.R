## R script to provide 2 functions: 1 to store a matrix in a variable as cache,
# and another to compute its inverse and cache it as well       

## Caches matrix x

makeCacheMatrix <- function(x = matrix()) {
        # Internal variable for storing the inverted matrix        
        i <- NULL 
        # Overrides matrix x from whatever was in the argument to a new value.
        # Also clears cache variable for inverted matrix i.
        set <- function(y) { 
                x <<- y # x is the old matrix, y is the new matrix
                i <<- NULL # i is cleared
        }
        # Returns the value of matrix x
        get <- function() {x}
        # Sets the value for cache variable i to the argument inverse
        setInverse <- function(inverse) {i <<- inverse}
        # Returns the value of cached variable i
        getInverse <- function() {i}
        # Returns list representing this cached matrix.
        # A little inconvenient and less than elegant compared to other prog languages
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Inverts matrix x and caches it inside structure created by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        # Obtains contents of cached variable corresponding to the inverted matrix
        i <- x$getInverse()
        # If variable is not NULL, return its CACHED contents (inverted matrix x)
        # and control back to caller
        if(!is.null(i)) {
                message("getting cached solved matrix")
                return(i)
        }
        # If not, obtain contents of cached variable corresponding to matrix in argument
        data <- x$get()
        # Inverts matrix
        i <- solve(data)
        # Sets inverted matrix in cached variable
        x$setInverse(i)
        # return result, which can be assigned to another variable.
        i
}