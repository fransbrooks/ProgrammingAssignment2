## The two functions together compute the inverseof a matrix if it has not been computed yet. If it has been computed,
## the function returns the inverse of the matrix from the cache and it doesn't waste time repeating the computations
##to calculate the inverse of the matrix.

## Initialize the makeCacheMatrix 'class'.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solution) inv <<- (solution)
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Computes the inverse of the matrix and returns it, or it merely returns the inverse from the cache if it has 
##already been calculated.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
