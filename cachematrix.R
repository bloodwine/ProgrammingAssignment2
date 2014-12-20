## This file contains functions that will cache
## 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    matrixinverse <- NULL
    ## getter for the matrix
    get<-function() {
        x
    }
    ## setter for the matrix
    set <- function(y){
        x <<- y
        matrixinverse <<- NULL
    }
    ## getter for the matrix inverse
    getinverse <- function (){
        matrixinverse
    }
    ## setter for the matrix
    setinverse <- function(inverse){
        matrixinverse <<- inverse
    }
    ## returns a list of getter and setter functions
    list(get=get, set=set, 
         setinverse=setinverse,
         getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    matrixinverse <- x$getinverse()
    if(!is.null(matrixinverse)){
        # Returns the matrix inverse if it was previously cached
        message("getting cached inverted matrix")   
        return(matrixinverse)
    }
    else{
        # Compute and cache the matrix inverse if it was not previously cached
        message("caching matrix inverse")
        matrixinverse <- solve(x$get(),...)
        x$setinverse(matrixinverse)
        matrixinverse
    }
}
