## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # nolint: indentation_linter.

        # Set the matrix data
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        # Get the matrix data
        get <- function() x

        # Set the cached inverse
        setinverse <- function(inverse) inv <<- inverse

        # Get the cached inverse
        getinverse <- function() inv

        # Return a list of functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()

        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }

        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

### Test case
# Create a matrix and cache its inverse
m <- matrix(rnorm(16), 4, 4)
m_cache <- makeCacheMatrix(m)

# Compute and cache the inverse of the matrix
inverse <- cacheSolve(m_cache)
print(inverse)

# Retrieve the cached inverse
cached_inverse <- cacheSolve(m_cache)
print(cached_inverse)
