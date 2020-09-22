
## Hi all! Thank you for reviewing my assignment. The objective here is 
# to create a function (a pair of fuctions actually) that can cache
# the inverse of a matrix. This is important due to potentially time
# consuming disadvantages when computing over and over again instead of using
# the first result (asuming it does not change).
# In this assingment we work with the scoping rules of R 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x<<-y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <-function(inverse) {inv <<- inverse}
    getInverse <-function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


# This function creates a special "matrix" object that can cache its inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("cached data is used")
        return(inv)
    }
    mat <-x$get()
    inv <-solve(mat, ...)
    x$setInverse(inv)
    inv
    
        ## Return a matrix that is the inverse of 'x'
}
