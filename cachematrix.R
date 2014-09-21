## Corey Hart - R Programming - Assignment 2 - 2014-19-21
##-------------------------------------------------------
## cachematrix.R 
##
## This R script contains two functions inspired by 
## makeVector() and cachemean() by rdpeng. 
##
##makeCacheMatrix(matrix) 
##  - returs and caches the inverse of a matrix.
##
##cacheSolve(matrix)
##  - returns the cached inverse of a matrix if availiable, if
##  not it runs makeCacheMatrix() on the provided matrix.
##-------------------------------------------------------------


makeCacheMatrix <- function(x = matrix()) {
    ## Creates a list for the purpose of cacheing the valus of x and its inverse
    ## stored in $cachedMatrix for possible reuse.
    ## NOTE: This function is intened to be used with cacheSolve, which will
    ## populate the cashed anwers in the list returned by this function.
    
    cachedMatrix <- NULL

    ##we need to remember the matrix that was sent so if called again we can compare
    set <- function(matrixtosolve) {
        x <<- matrixtosolve
        cachedMatrix <<- NULL
    }

   ##empty function to return x
    get <- function() x

    setmatrix <- function(solve) cachedMatrix <<- solve
    
    getmatrix <- function() cachedMatrix
    
    ##return the matrix give to sove, the matrix that was set, and the inverses
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
} ##end  makeCacheMatrix


cacheSolve <- function(x, ...) {
    ## Solves or returns a cached answer for the inverse of a matrix (x) given 
    ## that x has be run though MakeCacheMatrix() first, so that a list has 
    ## been made with the value x and its inverse as returned from this function 

    ## check for a cached value given the input
    cachedMatrix <- x$getmatrix()
    if(!is.null(cachedMatrix)) {
        message("getting cached data")
        return(cachedMatrix)
    }
    else message("this is new, will now be cached")
    
    ## time to do the actual work and solve the matrix
    data <- x$get()
    cachedMatrix <- solve(data, ...)
    x$setmatrix(cachedMatrix)
    cachedMatrix

} ## end cacheSolve
