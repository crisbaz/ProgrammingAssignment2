## These two functions work together to calculate and store (cache) the inverse
## of a matrix.  The matrix is the argument of makeCacheMatrix, and the list
## created in makeCacheMatrix is the argument for the cacheSolve function.


## makeCacheMatrix : a function that caches the inverse of a matrix object 
## generated using cacheSolve.  Parallels MakeVector VERY CLOSELY

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setminv <- function(minv) m <<- minv
    getminv <- function() m
    list(set = set, get = get, setminv = setminv, getminv = getminv)
}


## cacheSolve is a function that recalls previously solved matrix inverse from 
## makeCacheMatrix. If cacheSolve doesn't find the matrix inverse in 
## makeCacheMatrix,it calculates the matrix inverse then stores it 
## using makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the matrix used as argument to
        ## makeCacheMatrix, above
    m <- x$getminv()
    if(!is.null(m)){
        message("getting cached inverse of the matrix")
        return(m)
    }
    
    data <- x$get()
    library(MASS)
    m <- ginv(data, ...)
    x$setminv(m)
    m
}