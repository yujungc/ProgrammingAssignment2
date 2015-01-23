## The following two functions basically allows the user to input a square matrix (only square matrix has inverse), and gives and output of its inverse matrix. If the matrix is newly input, then it calculates and store this value in the cache. However, if the value of the inverse matrix is already computed and stored for the input matrix, then calculation is skipped and the value is cached and return.

## the "makeCahceMatrix" function creates the matrix, and is a list of functions to perform the following:
## "set" : It sets the value of the matrix
## "get": get the value of the matrix
## "setinverse": get the value of the inverse matrix
## "getinverse": finally get the value of the inverse matrix.
makeCacheMatrix <- function(x = matrix) {
	i <- NULL

	set <-function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set= set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## the following function calculates the inverse of the matrix given by the "makeCacheMatrix" function. It first checks if the inverse matrix has already been calculated. If it is, it skips the computation and directly cache the inverse matrix in the variable "i" of "makeCacheMatrix" function. Otherwise, it calculates the inverse matrix of the input matrix and stores it in the cache "i". Finally, it returns the inverse matrix, "i".

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}