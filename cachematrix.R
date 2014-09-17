## These functions cache the inverse of a matrix. makeMatrixCache
## creates an object that will get a cached matrix or store a 
## matrix in the cache.
## cacheSolve is the way to interact with matrix cache.

## Example usage: 
## c=rbind(c(1, -1/4), c(-1/4, 1))  
## > c=rbind(c(1, -1/4), c(-1/4, 1))  
## > c
## > d<-makeCacheMatrix(c)

## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## > cacheSolve(d)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(mean) m <<- mean
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
