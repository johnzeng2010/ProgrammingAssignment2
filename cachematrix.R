## This is the assignment 2 of R programming class.
## The requirement for this is to design a way to reduce costly
## computation of matrix inversion by using cache of inverse of
## a matrix.

## Create a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(m) {
		x <<- m
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get, 
	setinverse = setinverse,
	getinverse = getinverse)
}


## Computer the inverse of the special "matrix" returned by 
## makeCacheMatrix above.  If the inverse has already been calculated,
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
