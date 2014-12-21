# This file contains functions that will cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    # The makeCacheMatrix function creates a special "matrix" object 
    # that can cache its inverse
    #
    # Args:
    #   x: An invertible matrix
    #
    # Returns:
    #   A list of getter and setter functions for the matrix and its inverse
    
    matrixinverse <- NULL
    # getter for the matrix
    get<-function() {
        x
    }
    # Setter for the matrix
    set <- function(y){
        x <<- y
        matrixinverse <<- NULL
    }
    # Getter for the matrix inverse
    getinverse <- function (){
        matrixinverse
    }
    # Setter for the matrix
    setinverse <- function(inverse){
        matrixinverse <<- inverse
    }
    # Returns getter and setter functions for the matrix and its inverse
    list(get=get, set=set, 
         setinverse = setinverse,
         getinverse = getinverse)

}

cacheSolve <- function(x, ...) {
    # The cacheSolve function computes the inverse of the special "matrix" 
    # returned by makeCacheMatrix. If the inverse has already been calculated 
    # (and the matrix has not changed), then the cachesolve will retrieve the 
    # inverse from the cache
    #
    # Args:
    #   x: The special "matrix" returned by makeCacheMatrix
    #
    # Returns:
    #   The inverse of the special "matrix" returned by makeCacheMatrix

    matrixinverse <- x$getinverse()
    
    if(!is.null(matrixinverse)){
        # Returns the inverse of matrix 'x' if it was previously cached
        message("getting cached inverted matrix")   
        return(matrixinverse)
    }
    else{
        # Compute and cache the inverse of matrix 'x' 
        # (if it was not previously cached)
        message("caching matrix inverse")
        matrixinverse <- solve(x$get(),...)
        x$setinverse(matrixinverse)
        matrixinverse
    }
}
