## Matrix computations eg. inversions are CPU intensive computations. To  
## minimize the repeated computations we can used the previously computed
## data if there are no changes to the original data.

## makeCacheMatrix: 
## Takes a square inversible matrix as a parameter (assumption)
## Creates and initializes the variables used for storing the output 
##i.e. sets and gets inverse of a matrix, gets and sets the input matrix
## returns a list of the functions to operate on the matrix objects

makeCacheMatrix <- function(x = matrix()) {
    inversedMatrix <- NULL
    set <- function(y) {
        x <<- y
        inversedMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inversedMatrix <<- inverse
    getinverse <- function() inversedMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##cacheSolve: 
## Takes a square inversible matrix as a parameter (assumption)
## tries to get the already solved matrix from cache else computes the inverse 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
