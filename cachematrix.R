## Calculate the inverse of a non-singular square matrix x by calling 
## cacheSolve(makeCacheMatrix(x))

## Return a list to
##     1. Set the value of the matrix
##     2. Get the value of the matrix
##     3. Set the value of the inverse
##     4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Declare inverse matrix to be NULL to check if inverse is cached
    i <- NULL
    ## Cache the matrix and inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## Return matrix
    get <- function() {
        x
    }
    ## Cache inverse
    setinverse <- function(inverse) {
        i <<- inverse
    }
    ## Return inverse
    getinverse <- function() {
        i
    }
    ## Return the setter-getter functions in a list
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Calculate the inverse of a matrix
## Check if cache of inverse is present, add it if not
cacheSolve <- function(x, ...) {
    ## Try to get cached inverse
    i <- x$getinverse()
    ## Check if inverse is cached
    if (!is.null(i)) {
        message("getting cached data")
        ## Return cached inverse
        return (i)
    }
    ## Get matrix
    data <- x$get()
    ## Solve for inverse
    i <- solve(data, ...)
    ## Store cached inverse
    x$setinverse(i)
    ## Return a matrix, i, that is the inverse of 'x'
    i
}
