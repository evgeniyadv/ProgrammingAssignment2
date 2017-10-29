## Functions presented here are aimed to simplify calculations of inverse matrices in a loop
## The main idea is to cache the inverse untill the matrix is changed

## Function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invMatrix) inv <<- invMatrix
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function returns the inverse matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv <- solve(x$get())
        x$setinv(inv)
        inv
}
