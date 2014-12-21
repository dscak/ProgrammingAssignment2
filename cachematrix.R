## Creates a "matrix" (output is a list of functions to: 

	## 1. set the value of the matrix
	## 2. get the value of the matrix
	## 3. set the value of the inverse
	## 4. get the value of the inverse

##)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL # "resets" the inverse
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Returns the inverse of the matrix x. 
## If the inverse of x has been solved for already, cacheSolve retrieves it from the cache.
## If not, it computes it using solve().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() ## If inverse has not been computed yet, this will be NULL
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}