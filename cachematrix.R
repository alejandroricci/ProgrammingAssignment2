## Put comments here that give an overall description of what your
## functions do
#These two functions work together to allow you to save the inverse of a matrix along with it to avoid
#unnecesary recalculations of said inverse

## Write a short comment describing this function
#This function transforms a "regular" matrix into a pseudo-object (a list of functions) that contains
#said matrix but also can contain its inverse (once calculated).
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
#This function calculates the inverse of a "cache matrix" created by the previous function, but first it checks
#if the inverse wasn't already calculated, in which case just returns said inverse. If the inverse wasn't calculated,
#the function calculates the inverse matrix using the solve function and saves it in the "cache matrix" pseudo-object.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
