## The functions below implement Assignment 2 of the
## Coursera R Programming Language class (rprog-016).
## The goal is to create functions to cache the inverse of a matrix
## so as to save computation time.

## This function creates a inverse-cacheable matrix object

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function returns the inverse of the inverse-cacheable matrix
## created with the makeCacheMatrix function.
## If the inverse has already been determined, then it will
## simply return the cached inverse.  Else, it will compute
## the inverse and save it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}



test_assignment2 <- function() {
	m1 <- matrix( c(4, 7,  2, 6), nrow = 2, ncol = 2, byrow = TRUE)
	m1_inv <- solve(m1)

	m1c <- makeCacheMatrix(m1)
	m1c_inv <- cacheSolve(m1c)

	if (all.equal(m1_inv, m1c_inv)) {
	  print("m1_inv and m1c_inv are identical")
	} else {
	  print("m1_inv and m1c_inv are NOT identical")
	}

	m1c$set(m1_inv)
	m1c_inv_inv <- cacheSolve(m1c)

	if (all.equal(m1c_inv_inv, m1)) {
	  print("m1c_inv_inv identical")
	} else {
	  print("m1c_inv_inv NOT identical")
	}
}


