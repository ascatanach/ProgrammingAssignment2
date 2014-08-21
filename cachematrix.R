## Function to place an inverted matrix into cache
## This function takes a matrix as an argument and returns a list of functions that are 
## defined in the function body. This list of functions is accessed by the function 
## cacheSolve() (and other functions as necessary). 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL               ## i to take the inverse of the matrix, reset to NULL each time function
                                ## is called with a "new" matrix so that solve() can be called
        set <- function(y) {    ## this is not called by cacheSolve() but may be used to reset the original matrix
                                ## or could be used by other functions in the future.
                x <<- y         ## a matrix is passed as an argument and "superassigned" to cache
                i <<- NULL      ## i to take the inverse of the matrix when set() is called 
                                ## is cached using the superassignment operator
        }
        get <- function() x             ## returns the original matrix passed as argument
        setinverse <- function(inverse) i <<- inverse   ## the inverse of the matrix is passed from cacheSolve() 
                                                        ## and cached to the parent environment
        getinverse <- function() i                      ## returns the inverse, is NULL the first time 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    ## returning the list of functions
}                                                                                       ## to be called by other functions
                                                                                        ## including cacheSolve()



## cacheSolve takes the original matrix as an argument (and other optional arguments that can be 
## passed to solve()) and returns its inverse, from cache if it has been cached

cacheSolve <- function(x, ...) {
        i <- x$getinverse()             ## returns i which is either NULL or is the cached inverse
        if(!is.null(i)) {               ## if it isn't NULL then...
                message("getting cached data")      
                return(i)               ## return i from the cache ie from getinverse()
        }
        data <- x$get()                 ## but if i was NULL then get the original matrix...
        i <- solve(data, ...)           ## and find the inverse of it...
        x$setinverse(i)                 ## store it in cache using setinverse()...
        i                               ## and return it.
}