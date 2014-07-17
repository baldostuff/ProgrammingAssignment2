## Set of functions to enable caching of matrix inverse solutions


## Creates a matrix object that is capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Looks to see if matrix inverse has been cached
## If so, returns inverse from cache
## If not, calculates, stores inverse in cache and returns inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    # Check cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # Otherwise calculate solution
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}