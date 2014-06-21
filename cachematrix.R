## ----------------------------------------------------------------------------------
## makeCacheMatrix
## - parameters:
##    - x: matrix object defaulting to an empty matrix if omitted
## - output:
##    list object of functions for cacheable matrix operations
##    - set / get:  sets or gets the enclosed matrix
##    - setInverse: sets an internal variable with the inverse of the enclosed matrix
##    - getInverse: retrieves the inverse of the enclosed matrix
##
## - notes
##    - the "enclosed" matrix is the matrix parameter initially passed as a parameter
##      and bound via closure to the object created by makeCacheMatrix(matrix).  This
##      value is changed via the set(matrix) function.
## ----------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
        ## lexically scoped variable caching inverted matrix result
        m <- NULL
        
        ## update input matrix scoped by closure with new matrix value
        ## and reset the inverted matrix cache
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        ## get input matrix scoped by closure
        get <- function(){
                x
        }
        
        ## update cache with inverted matrix
        setInverse <- function(im){
                m <<- im
        }
        
        ## get cached inverted matrix
        getInverse <- function(){
                m
        }
        
        ## return a list object of functions bound to the input matrix via closure
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## ----------------------------------------------------------------------------------
## cacheSolve
## - parameters:
##    - x: makeCacheMatrix list result object
## - output:
##    inverse of parameter x matrix
##
## - notes
##    - if x does not have a cached inverse, the inverse is computed via the solve
##      function and stored in x via x's setInverse(im) function
##    - currently assumes a square invertable matrix
## ----------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## get the cached inverse if the matrix
        tmp <- x$getInverse()
        
        ## if the cached inverse is NOT null, we indeed have the cached inverse
        ## so go ahead and return it
        if (!is.null(tmp)) {
                message("getting cached data")
                return(tmp)                
        }
        
        ## we do not have the inverse cached yet
        ## get the matrix
        tmp2 <- x$get()
        
        ## solve for the inverse of that matrix
        tmp <- solve(tmp2, ...)
        
        ## cached the result
        x$setInverse(tmp)
        
        ## and return the result
        tmp 
}
