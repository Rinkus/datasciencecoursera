## This whole function is to cache the inverse of a matrix

## makeCacheMatrix is a list of function to make and set an inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setMatrix <- function(y) {      #this function to set a matrix into variable x
        x <<- y
        i <<- NULL
    }
    
    #this function return the matrix set before
    getMatrix <- function() x
    
    #this function to set an inversed matrix to variable i
    setMatrixInverse <- function(inverse) 
        i <<- inverse
    
    #this function return the inversed matrix
    getMatrixInverse <- function() 
        i
    
    #return makeCacheMatrix as a list of functions
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse= getMatrixInverse)
}


## cacheSolve is a function to calculate the inversed matrix and return 
## the cache inversed matrix if the data doesn't change

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    i <- x$getMatrixInverse()
    if(!is.null(i)) {
        message("getting cached inversed matrix")
        i
    }
    
    ## Calculate the inversed matrix
    data <- x$getMatrix()       #get the matrix into data variable
    i <- solve(data)            #Inversed the matrix
    x$setMatrixInverse(i)       #Set the inverted matrix
    i                           #return the inverted matrix
}
