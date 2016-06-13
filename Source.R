# makeCacheMatrix creates a list containing a function to set/get the value of the matrix/inverse one

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}
# in case of the inverse not being computed, this function compute the inverse 
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
m <- makeCacheMatrix(matrix(c(3, 4, 4, 3), c(2, 2)))
cacheSolve(m)
# sample result:
#>          [,1]       [,2]
#> [1,] -0.4285714  0.5714286
#> [2,]  0.5714286 -0.4285714
