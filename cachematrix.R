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
    
    ## cache need to be empty until cacheSolve give it a value.
    cachedMatrix <- NULL

    ##Populate elements of the list, current matrix, previous matchs, inverses 
    set <- function(matrixtosolve) {
        x <<- matrixtosolve
        cachedMatrix <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) cachedMatrix <<- solve
    getmatrix <- function() cachedMatrix
    
    ##return the list
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    
} ##end  makeCacheMatrix


cacheSolve <- function(x, ...) {
    ## Solves or returns a cached answer for the inverse of a matrix (x) given 
    ## that x has be run though MakeCacheMatrix() first, so that a list has 
    ## been made with the value x and its inverse as returned from this function 

    ## check for a cached value given the input, and return if found
    cachedMatrix <- x$getmatrix()
    if(!is.null(cachedMatrix)) {
        message("getting cached data")
        return(cachedMatrix)
    }
    else message("this is new, will now be cached")
    
    ## If there isn'ta a cached answer, solve the matrix and add to cache list.
    data <- x$get()
    cachedMatrix <- solve(data, ...)
    x$setmatrix(cachedMatrix)
    
    ## return answer (matrix inverse)
    cachedMatrix

} ## end cacheSolve
