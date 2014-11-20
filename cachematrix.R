## Two functions makeCacheMatrix and cacheSolve that together allow for solving the inverse of
## a matrix and caching the result

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    #########
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse= getinverse)
}


## cacheSolve() returns the inverse of the special matrix object x, either loading it from 
## a cached version or solving and saving to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
   
}
