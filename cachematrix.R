## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function will create a matrix and cache the value of invert of the matrix
# Using similar approach in README.md file, the function set the value of,
# the matrix, then get the value, and after that it will set the invert, and
# set the value
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #default invert is null (not being calculated at the beginning)
    set <- function(y) { 
        x <<- y
        inv <<- NULL #reset the inverse if matrix is reset
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

#This function calculate the invert of the matrix that created by previous fx
#If the invert of the matrix was calculated already, it will get the cached data
#If not, it will calculate, and cache the value
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) 
    }
    data <- x$get()  # Get the matrix
    inv <- solve(data, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the inverse
    inv  # Return the inverse
}
