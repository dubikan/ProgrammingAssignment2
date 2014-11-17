## Two functions to recieve a matrix, solve its inverse, and cache it for 
## subsequent quick retrieval

## makeCacheMatrix receives a matrix and returns a list containing four 
## functions to manipulate it, cache the inverse and retrieve it.

makeCacheMatrix <- function(x = matrix(),...) {
    solvedm <- NULL     ## Initiaize internal solved matrix
    set <- function(y) {    ## externally change the matrix
        x <<- y
        solvedm <<- NULL
    }
    get <- function() x     ## return matrix
    setinverse <- function(inverse) solvedm <<- inverse 
                                    ## cache inverse to parent environment
    getinverse <- function() solvedm    ## return cached inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve retrieves the cache's inverse from makeCacheMatrix. If no inverse 
## is cached, it solves the inverse and caches it using makeCacheMatrix, before
## returning the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    solvedm <- x$getinverse()   ## Check cached inverse
    if(!is.null(solvedm)) {     ## if cached inverse exists, return it
        message("getting cached data")
        return(solvedm)
    }
    data <- x$get()     ## if no cached inverse exists, get matrix and solve it
    solvedm <- solve(data, ...)
    x$setinverse(solvedm)   ## call to cache solved inverse
    solvedm     ## return inverse
}
