## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## use solve method to cache matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #reset the values
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #gets the matrix
    get <- function() x
    #assigns the matrix to parent environemnt
    setSolve <- function(solvedResult) m <<- solvedResult
    #gets the result
    getSolve <- function() m
    #assigns everything in list for easy access    
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #tries to fetch the cached inverted matrix from parent environment
    m <- x$getSolve()
    #if it already exists, then return the result with message
    if(!is.null(m)){
       message("getting cached data")
        return (m)
    }
    #if not get the source matrix
    data <- x$get()
    #apply solve to it to get the inverse
    m <- solve(data)
    #assigns to parent environment
    x$setSolve(m)
    #return results
    m
}
