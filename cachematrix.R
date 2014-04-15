## function makeCacheMatrix takes a matrix as its input and works similarly 
## to the makeVector example

## function cacheSolve checks to see whether the inverse of this matrix has been
## computed and if so retrieves it from the cache - otherwise it computes it

## This function first clears the inverse, then creates a list of functions:
## - gets the value of the matrix: used by the cacheSolve function to calculate the 
##                                 inverse
## - sets the value of the inverse: used by the cacheSolve functon to cache the 
##                                 inverse
## - gets the value of the inverse: used by the cacheSolve function if the inverse
##                                 already exists
## It is based on the makeVector function

makeCacheMatrix <- function(x = matrix()) {
        matInv <- NULL

        getMat <- function() x
 
        setInv <- function(inv) matInv <<- inv

        getInv <- function() matInv

        list (getMat = getMat, setInv = setInv, getInv = getInv)
}


## checks whether the inverse has been cached 
## - if so it retrieves it from the cache
## - otherwise it computes it and caches it

cacheSolve <- function(x, ...) {
        matInv <- x$getInv()

        if (!is.null(matInv)) {
                message("Inverse has been cached")
                return(matInv)
        }
        
        mat <- x$getMat()
        
        matInv <- solve(mat)
        
        x$setInv(matInv)
        
        matInv
}
