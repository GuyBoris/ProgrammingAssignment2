## Put comments here that give an overall description of what your
## functions do
# Prerequisites:
#     1. inverse of a matrix M is given by solve(M) (we suppose M has an inverse here,  we don't catch errors if it has none)
#     2. the functions depend on each other. makeCacheMatrix has to be used by cacheSolve in order to make sense.
#     3. the main point (as far as I understand) is that these functions illustrate where variables (or objects in general) 
#     are stored in function calls. The object "inv" from makeCachematrix gets set only by a call in cacheSolve - but the 
#     calculation is only carried out once for the instance x (in cacheSolve); the inverse is then cached inside the 
#     makeCacheMatrix object "inv". 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invMat) inv <<- invMat
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}