## Two functions to be used together to compute the inverse of a matrix, 
## and store/retrieve inverse from memory.
## 1.   makeCacheMatrix: Returns an object that can cache original matrix and its inverse
## 2.   cacheSolve: Computes and caches the inverse of matrix from the object returned by makeCacheMatrix.
##	    This function will retrieve the inverse (instead of resolving) if it is already in the cache.  


## Write a short comment describing this function
## makeCacheMatrix: This function takes in a matrix x and 
## creates a list object of 4 functions that is used to cache the original matrix (x)
## and its inverse (mi). 

makeCacheMatrix <- function(x = matrix()) { # initialize function argument matrix to be solved
        mi <- NULL					        # initialize object to store solution
        set <- function(y) {				
                x <<- y
                mi <<- NULL					# reset any previous values
        }
        get <- function() x
        setinv <- function(inv) mi <<- inv
        getinv <- function() mi
        list(   set = set, 					# name set() function in list object
                get = get,					# name get() function in list object 
                setinv = setinv,			# name setinv() function in list object
                getinv = getinv)			# name getinv() function in list object
}



## Write a short comment describing this function
## cacheSolve: This function takes the makeCacheMatrix list object as an argument instead of a numeric 
## matrix. It checks in the cache for the matrix inverse and returns that; 
## if no previous solution is found, the inverse is computed and cached in the list object.

cacheSolve <- function(x, ...) {                # takes object returned by makeCacheMatrix
        mi <- x$getinv()
        if(!is.null(mi)) {					    # if inverse is already stored in cache
                message("getting cached data")  # retrieves and returns inverse from cache
                return(mi)
        }
        data <- x$get()						    # retrieve matrix to solve
        mi <- solve(data, ...)					# compute inverse of matrix
        x$setinv(mi)                            # cache inverse
        mi
}


## Define test matrices

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

m2 <- matrix(c(1, 2, 3, 5), nrow = 2, ncol = 2)

m3 <- matrix(c(-3, 8, 10, -3, 7, 9, 2, -4, -5), nrow = 3, ncol = 3)
