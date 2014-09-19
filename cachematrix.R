## store a cahced version of a matrix the first time it is required
## so next time it will be fetched without doing an expensive matrix
## inverse operation

## create a chached matrix object holding :

## - the inverse of the matrix
## setter & getter of the matrix
## setter & getter of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)        
        
}


## function gets the inverse object, if its NULL,
## does the actual inverse and stores it in the
## chached matrix object
## If inverse matrix isn't NULL, return it without
## doing a matrix inverse
##


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv        
        
}
