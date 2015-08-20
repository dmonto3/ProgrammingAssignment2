## These two functions cache the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse.
## This function allows: 
## 1. Getting the data of the matrix.
## 2. Setting the data of the matrix.
## 3. Getting the inverse of the matrix.
## 4. Setting the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(y){
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix.
## If the inverse has already been calculated then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        return(inverse)
    }
    ## If is NULL the inverse must be calculated
    mymatrix <- x$getMatrix()
    inverse <- solve(mymatrix, ...)
    x$setInverse(inverse)
    inverse
}
