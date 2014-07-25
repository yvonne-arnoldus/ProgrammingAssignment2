## Put comments here that give an overall description of what your
## functions do
## using the mass libary cause that has a better inverse matrix function.
## cause ginv can inverse any matrix and solve can't
library('MASS')

## Write a short comment describing this function
## this function creates a special matrix. The matrix is a list contaning
## 4 functions: get (to get the matrix), set (to set the matrix), 
## getinverse (which gets the inverse matrix of the matrix, which is stored) and
## setinverse (which stores the inverse matrix of the matrix)
makeCacheMatrix <- function(x = matrix()) {
    ## stored inverse
    inv <- NULL
    
    ## set the matrix to invert
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get the matrix
    get <- function() x
    
    ## set the inverse of the matrix
    setinverse <- function(i = matrix()) inv <<- i
    
    ## get the inverse of the matrix
    getinverse <- function() inv
    
    ## creat the special matrix as a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
## This function excepts an input of makeCacheMatrix special matrix and returns
## the inversed matrix. If the inverse matrix was already stored on the special matrix
## this function will retrieve it from stored location. If it isn't stored
## then the inverse is calculated, stored and returned
cacheSolve <- function(x, ...) {
    
    ## check to see if the special matrix has the inverse already stored
    inv <- x$getinverse()
    
    ## the inverse was already stored in the special matrix, so return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## the inverse wasn't already stored in the special matrix, so calculate it
    ## get the origional matrix
    matrix <- x$get()
    
    ## calculate the inverse, using ginv from the MaSS library
    inv <- ginv(matrix)
    
    ## store the calculated inverse within the special matrix.
    x$setinvers(inv)
    
    ## return the inverse matrix
    inv
}
