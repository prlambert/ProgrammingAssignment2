## All implementation in this file done by Paul Lambert
## Github: https://github.com/prlambert/ProgrammingAssignment2/

## The two functions in this file work together to compute and cache values of 
## an inverted matrix.
##
## The first function, 'makeCacheMatrix' returns a list with 'getter'
## and 'setter' functions for both a matrix and its cached inverse. These
## functions have access to the environment inside the 'makeCacheMatrix'
## closure and thus are able to store values unique to each list 
## (not accessible globally), but scoped only to the environment inside the 
## 'makeCacheMatrix' function closure.
##
## cacheSolve returns an inverted version of a matrix stored in a list structure
## created by the 'makeCacheMatrix' function. The underlying matrix that
## is inverted must first be set by passing it as a parameter to the 
## 'makeCacheMatrix' such as: "matrixList <- makeCacheMatrix(underlyingMatrix)" 
## or by calling matrixList$set(underlyingMatrix) (setting explicitly on an 
## existing list structure). If cacheSolve has been previously called on the 
## same, unmodified, list structure, a cached value of the inverted matrix will
## be returned, thereby skipping unnecessary processing time. If this is the 
## first time cacheSolve has been called for the given list structure and 
## underlying matrix, the inverted matrix will be computed and stored in the
## list, cached for future use. 


## makeCacheMatrix takes an optional underlying matrix as it's sole parameter
## and returns a list structure with a series of 'getter' and 'setter'
## functions that have access to its closure environment, as described at the
## top of this file.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() { 
        x
    }
    setInverse <- function(i) {
        inverse <<- i  
    } 
    getInverse <- function() {
        inverse
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns a inverted matrix, provided its preconditions are met.
## These preconditions include that the parameter passed is a list structure
## as constructed by the makeCacheMatrix function and the underlying matrix
## is invertible. If cacheSolve has been previously called on the passed
## (unmodified) list structure, a cached value for the inverted matrix will
## be returned. Otherwise, the inverted matrix will be calculated by the 'solve'
## function and cached for future use.
## See the comment at top of this file for more information. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("getting cached inversed matrix")
        return(inverse)
    }
    
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
