## This functions help to find the inverse matrix of a matrix

## contains a list of functions that allow you to enter, store, call a function of your cache memory. 
##On the other hand it also allows to do the same with the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inve <- NULL
        set <- function(y) {
           x <<- y
           inve <<- NULL
        }
        get <- function() x
        setInverse <- function(inversa) inv <<- inversa
        getInverse <- function() inve
        list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function allows to verify if the inverse matrix of a function is stored in 
##the cache memory and if it is not found, it calculates it and stores it in the cache.

cacheSolve <- function(x, ...) {
        inve <- x$getInverse()
        if (!is.null(inve)) {
           message("getting cached data")
           return(inve)
        }
        matriz <- x$get()
        inve <- solve(matriz, ...)
        x$setInverse(inve)
        inve
}
