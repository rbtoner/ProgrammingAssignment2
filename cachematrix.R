## cachematrix.R:
## Set of functions designed to cache the computation of a matrix's
## inverse, to avoid potentially time-consuming recalculations.
## First function (makeCacheMatrix) takes a numeric matrix and creates
## a "vector" of functions to access and set the matrix and its inverse.
## Second function (cacheSolve) takes the "vector" and either calculates
## and then caches and returns the inverse (if no inverse has been
## calculated yet), or simply returns the inverse from the cache otherwise.

## makeCacheMatrix: Takes a matrix (x) and creates a "vector" of the
## following functions:
## 1. set: Set the value of the matrix (set to x automatically).
## 2. get: Get the value of the matrix.
## 3. setinv: Set the inverse (do not use this interactively
##         if you later plan on using cacheSolve to get inverse).
## 4. getinv: Get the value of the inverse (set to NULL by default)

makeCacheMatrix <- function(x = matrix()) {

    #Inverted matrix; set to NULL in current environment
    im <- NULL

    #Function to define matrix x (and set im to NULL)
    set <- function(y) {
        x <<- y
        im <<- NULL
    }

    #Function to return the matrix:
    get <- function() x

    #Function to set the inverted matrix manually:
    #Note: do not use this if you intend to use
    #cacheSolve(x) later, as it will supersede the
    #inverse being stored.
    setinv <- function(inv) im <<- inv

    #Function to return the inverted matrix:
    getinv <- function() im

    #Return a list with all of these functions:
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve: Calculate inverse matrix from the "vector" (x)
## output by the above function makeCacheMatrix.
## Function first checks if the inverse has been defined (via
## getinv()). If it has already been defined, the stored value
## is returned from the cache (rather than being calculated again.
## If the stored inverse matrix is NULL, the function calculates
## the inverse from the stored data and caches it before returning.

cacheSolve <- function(x, ...) {

    #Grab the current inverse in the makeCacheMatrix list.
    im <- x$getinv()

    #Has the inverse already been set?  If so, return stored data:
    if(!is.null(im)) {
        message("Getting cached data.")
        return(im)
    }

    #Otherwise, calculate and cache the inverse.
    #Grab the matrix to be inverted:
    data <- x$get()
    #Solve for the inverse matrix:
    im <- solve(data, ...)
    #Store the inverse matrix:
    x$setinv(im)
    #Return the value:
    im
}

