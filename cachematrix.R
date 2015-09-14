## makeCacheMatrix has four functions to store cache, retrieve cache, store matrix, retrieve matrix
## set = stores or replaces a new matrix
## get = retrieves the matrix
## setcache = sets a new inverse matrix
## getcache = stores the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setcache <- function(cache) m <<- cache
    getcache <- function() m
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)
}


## cacheSolve either solves the matrix if there is no cache value or returns the inverse matrix

cacheSolve <- function(x, ...) {
    m <- x$getcache()
    mat <- x$get()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    m <- solve(mat, ...)
    x$setcache(m)
}