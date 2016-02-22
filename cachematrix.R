## makeCacheMatrix:
## To facilitate this caching, you first create a special
## matrix that will help us with this by using the
## makeCacheMatrix function.  The input into this function
## is simply a variable of type matrix.
##
## Usage example:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
    # Following the same format as the example given with the instructions,
    # creating a makeCacheMatrix object will consist of
    # four functions encapsulated in a list
    # 1. set the matrix
    # 2. get the matrix
    # 3. set the inverse of the matrix
    # 4. get the inverse of the matrix
    
    # Initialising the  'inv' variable to NULL
    # This changes when the user sets this by caching it.
    inv <- NULL
    
    # set function which sets the original matrix(not the inverse).
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get function which gets the original matrix(not the inverse). 
    get <- function() x
    
    # Setting the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Getting the inverse
    getinverse <- function() inv
    
    # Put this in a list which represents our cache data structure.
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## cacheSolve:
## The cahceSolve Matrix can be obtained to get the inverse
##
## ##
## Usage example:
## x <- matrix(1:4, nrow=2, ncol=2)
## k <- makeCacheMatrix(x)
## s <- cacheSolve(k)
## print(s)
## s should return:
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## s2 <- cacheSolve(k)
## This should display a "Getting cached matrix" message
## print(s2)
## s2 should return
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

cacheSolve <- function(x, ...) {
    ## This function is meant to return a matrix that is the inverse of it's first argument.
    # Following the same format as the example given with instructions,    
    # this gets the current state of the inverse and see if it
    # has been computed yet. 
	#If it is not computed yet, it computes the inverse and returns it.
	# If the inverse already exists, it retrieves the existing value and returns it. 
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        # Return the computed inverse
        message("Getting cached matrix")
        return(inv)
    }    
    data <- x$get()
    
	# finds the inverse if there is no existing inverse of matrix.
    inv <- solve(data, ...)
    
    # Before returning, we cache this result.
    x$setinverse(inv)
    
    # returning the result.
    inv
}