## Calculate the inverse of a non-singular square matrix x by calling 
## cacheSolve(makeCacheMatrix(x))

## Return a list to
##     1. Set the value of the matrix
##     2. Get the value of the matrix
##     3. Set the value of the inverse
##     4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() {
        i
    }
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Calculate the inverse of a matrix
## Check if cache of inverse is present, add it if not
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return (i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    ## Return a matrix, i, that is the inverse of 'x'
    i
}
