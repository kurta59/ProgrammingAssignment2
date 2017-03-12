## kca
## The inversion of a matrix is costly and there is benefit to
## caching the inverse of matices rather than repeat running of the 
## computation

## This function creates a special "matix" object that can
## cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


## This function computes the inverse of the "matrix" created 
## in function makeCacheMatrix.  If the inverse has been 
## calculated and is unchanged then the inverse is retrieved 
## from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
