## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is storing the "cached" copy as the local variable "m".
## The non-inverted matrix is stored as "x". 
## The output from this function is a list with 4 functions: get(). set(), getInverse(), setInverse()
## The calling functions is responsible for calculating the inverse, by first get-ing the non-inverted matrix
## then calling the "solve()" function on it, then storing it back into the cache.


makeCacheMatrix <- function(x = matrix()) {  ## "x" is the non-inverted matrix
	m <- NULL  ##  this is the cached copy of the inverted matrix
	
	set <- function(y) {  ## replace the current non-inverse matrix with the one passed in (as "y")
		x <<- y
		m <<- NULL
	}
	
	get <- function() x  ## return non-inverted matrix
	setInverse <- function(inv) m <<- inv
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function takes as argument the list returned by makeCacheMatrix.
## This function then checks the cached copy ("m"), which if it exists, assumes is the cached copy that it needs
## if, it is null, assumes that the inverse has not yet been calculated. It then calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
