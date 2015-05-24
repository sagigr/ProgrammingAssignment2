## Put comments here that give an overall description of what your
## functions do


##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                   
                   invmat <- NULL
                   
                   setmat <- function(y){
                          matrix <<- y
                          invmat <<- NULL
                   }
                   
                   getmat <- function(){
                          matrix
                   }
                   
                   setinvmat <- function(invm) {
                              invmat <<- invm
                   }
                   
                   getinvmat <- function() {
                             invmat
                   }
                   
                   list(setmat = setmat, getmat = getmat,
                        setinvmat = setinvmat,
                        getinvmat = getinvmat)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invmat <- x$getinvmat()
        
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
                
        ##If the inverse wasn't yet been calculated
        
        datamat <- x$getmat()
        
        matinv <- solve(datamat) %*% datamat
        
        x$setinvmat(matinv)
        
        matinv
}
