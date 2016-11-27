## The below functions allow the inverse of a matrix to be cached.  This
## means that this time intensive task can be done once upfront, and then
## the result of this inversion called on in future

## makeCacheMatrix sets up a framework in which the inverted matrix can be stored
## 'm' the inverted matrix, is initially empty, but setInv can be used to set m
## as the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}
## CacheSolve takes a list of the same format as the output of makeCache as an argument
## On the first call it will calculate the inverse of the matrix, and store it
## as m in the input Object.  In subsequent calls it will skip the calculation
## In both cases, the inverted matrix is returned
cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}