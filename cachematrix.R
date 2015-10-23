## These functions create the structure to cache values
## in a list and use this structure to invert certain initial matrix.

## this function takes a Matrix and creates
## a list containing access functions to this matrix and its inversion!

makeCacheMatrix <- function(initialMatrix = matrix()) {
        #Initially sets the inversed matrix to null
        inversed <- NULL
        
        #define a set function for the main matrix
        set <- function(received) {
                initialMatrix <<- received
                inversed <<- NULL
        }
        
        #define a get function for the main matrix
        get <- function() {
                initialMatrix
        }
        
        #define a set function to inversed matrix
        setinverse <- function(inverse) {
                inversed <<- inverse
        }
        
        #define a get function to the inversed matrix
        getinverse <- function() {
                inversed
        }
        
        #returns a list containing all functions:
        list(
                set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
        
}


## This function takes a list containing functions to access
## the initial matrix and its inversion.
## If the inversion has not been calculated so the
## function will calculate it.
## If already calculated then the function will use the cached value.

cacheSolve <- function(initialMatrixList, ...) {
        ## Return a matrix that is the inverse of 'initialMatrix'
        
        ##get the inversed in the list
        inversed <- initialMatrixList$getinverse()
        
        #check if it exists
        if (!is.null(inversed)) {
                return(inversed)
        }
        
        #if inversed not exists:
        
        #gets the initial matrix
        data <- initialMatrixList$get()
        
        #invert the initial matrix
        inversed <- solve(data, ...)
        
        #sets the inverted matrix in the list
        initialMatrixList$setinverse(inversed)
        
        #return the inversed:
        inversed
}