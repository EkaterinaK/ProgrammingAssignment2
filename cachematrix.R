## these 2 functions work together to cache the inverse of a matrix
## 1) first you need to create a special object ('matrix' object) 
## from a common matrix (use function makeCacheMatrix()). 
## this object will be stored in enclosed 
## environment.
## 2) then you can get inversed matrix by calling cacheSolve() and passing your 
## 'matrix object' to it

## This function creates and returnes a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    get <- function() x
    set_inversed <- function(i = matrix()) inversed <<- i
    get_inversed <- function() inversed
    list(set = set, get = get,
         set_inversed = set_inversed,
         get_inversed = get_inversed)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
    inversed <- x$get_inversed()
    if(!is.null(inversed)) {
        message("getting cached data")
        return(inversed)
    }
    data <- x$get()
    inversed <- solve(data, ...)
    x$set_inversed(inversed)
    ## Return a matrix that is the inverse of 'x'
    inversed
}
