## Put comments here that give an overall description of what your
## functions do


## A function to create a special matrix which can be cached

makeCacheMatrix <- function(x = matrix()) {
	
	inverseMatrix <- NULL
	set <- function(y){
		x <<- y
		inverseMatrix <<- NULL
	}
	
	get <- function() x
	setInverseMatrix <- function(invMatrix) inverseMatrix <<- invMatrix
	getInverseMatrix <- function() inverseMatrix
	list(set = set, get = get, setInverseMatrix = setInverseMatrix,
	getInverseMatrix = getInverseMatrix)
}


## A function that can compute inverses matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverseMatrix <- x$getInverseMatrix()
	if(!is.null(inverseMatrix)){
		return(inverseMatrix)
	}
	tempMatrix <- x$get()
	inverseMatrix <- solve(tempMatrix, ...)
	x$setInverseMatrix(inverseMatrix)
	inverseMatrix
}
