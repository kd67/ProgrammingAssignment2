## Caching the inverse of a matrix
## Calculating the inverse of a matrix is usually a costly operation and therefore, there may be certain benefits if we cache this operation

## This function creates a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y){
			x <<- y
			i <<- NULL
		}
		get <- function() x
		set_inverse <- function(inverse) i <<- inverse
		get_inverse <- function() i
		list( set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function computes the inverse of the matrix created by the function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get_inverse()
        if (!is.null(i)){
        	message("getting cached data")
        	return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$set_inverse(i)
        i
}
