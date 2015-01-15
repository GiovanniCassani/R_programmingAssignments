## This functions are meant to save time: since inverting a matrix can be very time consuming, 
# storing the inverse of a matrix in a different environment and retrieving it in case it is 
# required again can save time and make computations more efficient.
#

## The function makeCacheMatrix takes a numeric matrix as input (it needs be square and 
# non-singular) and  returns a list containing four functions: 
# - set(y) assigns the value null to the variable invMat
# - get() takes the value from the argument mat
# - setinverse(solve) apply the function solve to the input matrix
# - getinverse() takes the value assigned to invMat

makeCacheMatrix <- function(mat = matrix()) {
    invMat <- NULL
    set <- function(y) {
        mat <<- y
        invMat <<- NULL
    }
    get <- function() mat
    setinverse <- function(solve) invMat <<- solve
    getinverse <- function() invMat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes an object created through the function makeCacheMatrix as input and does 
# the following:
# - applies the function getinverse() to the object mat to get the inverse of mat from memory
# - if the object existed in memory, it warns that it is getting cached data and outputs the 
#   inverse
# - if nothing could be retrieved, it gets the matrix via the function get, applies the solve 
#   function to the matrix, sets the value and returns the inverse
   

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    invMat <- mat$getinverse()
    if(!is.null(invMat)) {
        message("getting cached data")
        return(invMat)
    }
    data <- mat$get()
    invMat <- solve(data, ...)
    mat$setinverse(invMat)
    invMat
}
