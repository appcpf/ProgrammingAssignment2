## R Programming
## --------------------------------
## Week 3: Programming Assignment 2
## --------------------------------

## ----------------------------------------------------------------
## The following function will store the inverse of a matrix.
## I have placed the step-by-step description inside each function.
## ----------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
        # First create variable (empty cache)
        InverseM <- NULL
        
        # Put the value of the matrix
        set <- function(y){
                # The <<- operator allows "set" to instantiate x outside of the function's environment
                x <<- y
                InverseM <<- NULL
        }
        
        # Get matrix value
        get <- function() x # returns x
        
        # Get the value of the inverse and store it in "inverse"
        setInverseM <- function(inv) InverseM <<- inv
        
        # Retrieve the value of the inverse
        getInverseM <- function() InverseM 
        
        # List functions so you can call them later
        list(set = set, get = get,
             setInverseM = setInverseM,
             getInverseM = getInverseM)
}


## ----------------------------------------------------------------
## The following function will get the inverse matrix in cache.
## I have placed the step-by-step description inside each function.
## ----------------------------------------------------------------

cacheSolve <- function(x, ...) {
        # Get the return value of the Inverse matrix
        InverseM <- x$getInverseM()
        
        # Condition to evaluate whether or not there is a value in the cache
        # If cache value is NOT null, get cache...
        if(!is.null(InverseM)){
                message("Getting Data in Cache...")
                return(InverseM)
        }
        # ...If IS null, then get the inverse (with solve()) and cache it
        value <- x$get()
        InverseM <- solve(value,...)
        x$setInverseM(InverseM)
        InverseM #returns the Inverse matrix
}
