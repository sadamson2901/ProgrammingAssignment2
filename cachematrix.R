# Matrix inversion is a costly computation.  This assignment will attempt to
# cache the results so future runs are faster

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y 
                m <<- NULL
        }
        
        get <- function() x
        setInverseMatrix <- function(inverse) m <<- inverse
        getInverseMatrix <- function() m
        
        list(set = set, get = get, setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("Cache hit!")
                return(m)
        }
        
        message("Cache miss!")
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        
        m
}
