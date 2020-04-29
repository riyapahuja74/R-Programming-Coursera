## Returns a list that contains the 2 pairs of getter and setter methods include:
## set the value of the vector
## get the value of the vector
## set the inverse value
## get the inverse value

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	## Setter for matrix
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	
	# Getter for matrix
	get <- function() x
	
	# Set Inverse Value
	setInverse <- function(inverse) m <<- inverse
	
	# Get Inverse Value
	getInverse <- function() m
	
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Calculate the inverse of the input value x and caches it
cacheSolve <- function(x, ...) {
	
	inverse <- x$getInverse()
	
	## return inverse if already calculated
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
	
	## calculate inverse
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
	
        inverse 
}
