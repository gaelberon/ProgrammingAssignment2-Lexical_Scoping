## The functions below are meant to return the inverse of a matrix
## One particularity is to return the data from the cache when it has already
## been calculated or calculates it and stores it in cache when not

## Example with vector
## makeVector <- function(x = numeric()) {
##         m <- NULL
##         set <- function(y) {
##                 x <<- y
##                 m <<- NULL
##         }
##         get <- function() x
##         setmean <- function(mean) m <<- mean
##         getmean <- function() m
##         list(set = set, get = get,
##              setmean = setmean,
##              getmean = getmean)
## }
## 
## cachemean <- function(x, ...) {
##         m <- x$getmean()
##         if(!is.null(m)) {
##                 message("getting cached data")
##                 return(m)
##         }
##         data <- x$get()
##         m <- mean(data, ...)
##         x$setmean(m)
##         m
## }


## The first function, makeCacheMatrix creates a special "vector",
## which is really a list containing a function to :
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function calculates the inverse of the special "vector"
## created with the above function. However, it first checks to see if the
## inverse matrix has already been calculated. If so, it gets the inverse
## matrix from the cache and skips the computation. Otherwise, it calculates
## the inverse matrix of the data and sets the value in the cache via the
## setsolve function. prior to that, the function performs a few checks :
## - is the matrix square or not?
## - is the matrix singular or not?

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("Getting inverse matrix from cache")
                return(m)
        }
        data <- x$get()
        if(nrow(data) != ncol(data)) {
                message("The stored matrix is not square and has no inverse!")
                ## Test if data is not a square matrix
        } else if(det(data) == 0) {
                ## Test if data is a singular matrix
                message("The stored matrix is singular and has no inverse!")
        } else {
                ## Calculate the inverse of the stored matrix and store
                ## the result in cache
                m <- solve(data, ...)
                x$setsolve(m)
                m
        }
}
