## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # see example Caching the Mean of a Vector
        # this program is almost identical, except matrix operations instead of
        # vector
        m <- NULL   # m will be the cached inverse matrix
        
        # 1. set the value of the matrix (similar to example)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # 2. get the value of the matrix (as in example)
        get <- function() x
        
        # 3. set the value of the inverse matrix
        setinv <- function(inverse) m <<- inverse

        # 4. get the value of the inverse matrix
        getinv <- function() m
        
        # return the matrix functions
        list(set = set, 
             get = get, 
             setinv = setinv, 
             getinv = getinv)        

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## see example cachemean        
        m <- x$getinv()
        
        #  has the inverse already been calculated? If so, returns it
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # since not calculate it
        # "if X is a square invertible matrix, then solve(X) returns its inverse"
        data <- x$get()
        m <- solve(data, ...)
        
        # sets the value of the inverse in the cache using setinv function from above.
        x$setinv(m)
        m
}
