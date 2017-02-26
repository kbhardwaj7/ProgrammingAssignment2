## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## matrix
        ## input to cacheSolve()
        
        inv = NULL
        set = function(y) {
             
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
                
        inv = x$getinv()
        
        # inverse is already there
        if (!is.null(inv)){
                # from cache no calculation
                message("from cache")
                return(inv)
        }
        
        # inverse is not there
        kmatrix.data = x$get()
        inv = solve(kmatrix.data, ...)
        
        
        x$setinv(inv)
        
        return(inv)
}
