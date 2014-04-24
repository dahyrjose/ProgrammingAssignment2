## This function uses solve() to calculate the inverse of a squared matrix and handles caching
## over that calculation in order to avoid innecessary computing effort

##
#Assignment: Caching the Inverse of a Matrix
#
#Matrix inversion is usually a costly computation and their may be some benefit to caching the 
#inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
#inversion that we will not discuss here). Your assignment is to write a pair of functions that 
#cache the inverse of a matrix.
#
#Write the following functions:
#	
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above. If the inverse has already been calculated (and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, 
#if X is a square invertible matrix, then solve(X) returns its inverse.
#
#For this assignment, assume that the matrix supplied is always invertible.
##


## This function handles the main functions to calculate the inverse of a matrix and helps in 
## determining when to erase cache
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	# If a new matrix is set the previous calculation is erased to avoid inconsistency of the 
	# result
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	# Retunring the original matrix
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## x must be constructed using makeCacheMatrix function
cacheSolve <- function(x, ...) {
	# Return a matrix that is the inverse of 'x' if it is cached
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("Returning cached inverted matrix")
		return(inv)
	}
	# If the inverse of the matrix is not cached then calculate it
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

## Try this sentences to test the code
# mat <- matrix(c(1, 2, 3, 0, 4, 5, 1, 0, 6), nrow = 3, ncol = 3)
# cacheSolve(makeCacheMatrix(mat))
# cacheSolve(makeCacheMatrix(mat)) %*% mat

