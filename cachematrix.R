## the following pair of functions are used to calculate the inverse of a matrix 
## and be able to reuse the inverse matrix without recomputing it (use of a cache function)

## Create a list that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invm) inv <<- invm 
    getinv <- function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## Return a matrix that is the inverse of the square invertible matrix 'x'

cacheSolve <- function(x, ...) {

    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    message("no cached data - compute the inverse of the matrix")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv

}
