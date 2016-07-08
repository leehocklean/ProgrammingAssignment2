## These following two functions together caches and retrieves the inverse 
## of the matrix. 

## The first function, makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## returns a list of functions for cacheing inverse of a matrix
    # set cache (inv) to NULL
    inv <- NULL
    
    # create a matrix (y) in the current environment
    # assign y to an object (x) in a different environment using <<- operator
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # return the value of the matrix (x)
    get <- function() x
    # store inverse of the matrix (x) in cache (inv)
    setInverse <- function(inverse) inv <<- inverse
    # retrieve the inverse of the matrix from cache.
    getInverse <- function() inv

    # Returns a list of functions for making a matrix and cacheing its inverse 
    list(set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}


## The following function computes the inverse of the matrix created with the
## above function. At first, it checks to see if the inverse has already 
## been computed. Otherwise it computes the inverse of the matrix and sets
## the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    ## This function returns a matrix that is the inverse of 'x'

    # Retrieves the inverse and assigns it to cache.
    inv <- x$getInverse()
    # Checks to see if the inverse is contained in cache
    if(!is.null(inv)) {
        message("getting cached data")

        # return the inverse of the matrix
        return(inv)
    }
    # Retrieves the matrix and assigns it to data
    data <- x$get()
    # Computes the inverse and assigns it to cache
    inv <- solve(data, ...)
    # Stores inverse in cache
    x$setInverse(inv)
    # Displays results in console
    inv
}
