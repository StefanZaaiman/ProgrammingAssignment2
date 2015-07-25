## The pair of functions below, makeCacheMatrix and cacheSolve saves time in calculating the inverse of 
## a matrix by caching the invese rather than computing it repeatedly.

## This function "makeCacheMatrix" creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	
	# set the value of the matrix
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	# get the value of the matrix
	get <- function() x

	# set the value of the inverse of the matrix
	setinverse <- function(solve) m <<- solve
	
	# get the value of the inverse of the matrix
	getinverse <- function() m
	list(set = set,
		 get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function "cacheSolve" computes the inverse of the special matrix object returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
	
	# check to see if the inverse has already been calculated
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached matrix data")
		return(m)
	}

	# if the above test shows there is no cached inverse proceed to calculate it:	
	data <- x$get()
	m <- solve(data, ...)
	
	# set the value of the calculated inverse in the cache
	x$setinverse(m)
	
	# return the inverse as the answer to the function cacheSolve
	m
}
