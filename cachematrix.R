##Returns a list of functions that will store the matrix and cache the matrix inverse.
makeCacheMatrix <- function(x=matrix()) { ##function assignment, argument must be a matrix
        library(MASS) ##load package mass as the inverse is calculated using ginv()
        inv <- NULL ##initialize inverse
        set <- function(y) {
                x <<- y ##cache function to store in x
                inv <<- NULL ##reinitialise inverse
        }
        get <- function() x ##function that will retrieve the matrix
        setmatrixinv <- function(ginv) inv <<- ginv ##set the matrix inverse
        getmatrixinv <- function() inv ##retrieve  the matrix inverse to be used in function x above
        list(set = set, get = get,
             setmatrixinv = setmatrixinv,
             getmatrixinv = getmatrixinv) #print list with functions outputs
}
##Calculate the inverse of the special matrix created with the function above makeCacheMatrix     
cacheSolve <- function(x, ...) {
        inv <- x$getmatrixinv() ##get matrix inverse
        if(!is.null(inv)) { ##check if there is changes to matrix. If not, get the cached matrix instead of recalculate
                message("getting cached matrix")
                return(inv) ##return inverse
        }
        data <- x$get() ##recalculate inverse and bring back to cache.
        inv <- ginv(data, ...)
        x$setmatrixinv(inv)
        inv
}
