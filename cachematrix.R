## Put comments here that give an overall description of what your
## functions do
## test
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setinv <-  inv <<- solve(m)
    getinv <- function() inv
    list(set = set , get =get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- m$setinv
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data)
    m$setinv(inv)
}
