## This file contains two functions, meant to speed the computation of a matrix inverse.
## makeCacheMatrix receives a matrix. If the inverse of the matrix was previously calculated, 
## 			 it enables the return of the inverse from cache memory
## cacheSolve receives the output from the makeCasheMatrix, i.e. a matrix, and, if it was solved previously,
##                 returns the solution from cache.  If not solved previously, it solves it and stores
##                 the solution in cache.


## Creates a list of functions used to identify new matrices, and store the inverse in cache. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    ## Now make the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## this accesses the functions created above, to either solve the matrix and store solution, 
## or just retrieve solution from cache memory. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
# To USE:
# z <- c(11,12,13,14)
# y <- matrix(z, 2,2)
# b <- makeCacheMatrix(y)
# c <- cacheSolve(b)
# Subsequent calls of c <- cacheSolve(b) returns the message that the solution was retrieved from cache.

