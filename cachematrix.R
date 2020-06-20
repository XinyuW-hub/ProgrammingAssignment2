## The whole cachematrix.R would first cache the 
##inverse of a matrix, and then return the inverse back
##to the original matrix.

## makeCacheMatrix would create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInv <- function(solve) s <<- solve
        getInv <- function() s
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve would compute the inverse 
##of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting inversed matrix")
                return(inv)
        }
        temp <- x$get()
        inv <- solve(temp, ...)
        x$setInv(inv)
        inv
}
