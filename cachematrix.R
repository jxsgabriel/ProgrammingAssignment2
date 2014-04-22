## Programming Assignment #2
## Program: cachematrix.R 
## Author : Jerel San Gabriel
## Write a pair of functions that cache the inverse of a matrix


## Return a list of functions that will allow
## caching of the matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## Set the value of the matrix to y
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the value of the matrix
        get <- function() x
        
        ## Set the inverse of the matrix
        setinv <- function(inverse) inv <<- inverse
        
        ## Get the inverse of the matrix
        getinv <- function() inv
        
        ## Return a list containing the four functions defined above
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Return a matrix that is the inverse of 'x'. 
## This function will check if an inverse has been cached.
## If not, then it will compute the inverse, anc cache it
## for future use.

cacheSolve <- function(x, ...) {
        
        
        ## Check if an inverse for the matrix has been cached
        inv <- x$getinv()
        
        ## If an inverse is cached, return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Otherwise, compute the inverse of the matrix
        mat <- x$get()
        inv <- solve(mat, ...)
        
        ## Cache the inverse
        x$setinv(inv)
        
        ## Return the inverse 
        inv
}
