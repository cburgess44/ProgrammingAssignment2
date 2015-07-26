## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function makeCacheMatrix <- function(x = matrix()) {}

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix() ) {
	
        i <- NULL ## start an inverse property
                
                set <- function(matrix) { 
                         x <<- matrix
                                i <<- NULL      ## set a matrix, inverse
        }
                get <- function() { 
                  	x                       ## get the matrix that was set
        }
                setInv <- function(inv) {
                        i <<- inv               ## set the inverse of the matrix
        }
                getInv <- function() { 
                        i                       ## get the inverse of the matrix that was set
        }
                list(set = set, get = get,
                        setInv = setInv,
                        getInv = getInv)        ## return the list of matrix and inverse
        }


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInv() ## get the inverse matrix of x
                
                if(!is.null(m) ) { ## if inverse is already set, then return m
                        message("getting cached data")
                        return(m) 
        }
                data <- x$get()  ## get the matrix from x
                
                m <- solve(data) %*% dat ## use solve to calculate the inverse of x; %*% multiplication of the matrix
                
                x$setInv(m) ## set the inverse
                
                m  ## return the matrix (inverse)
        }
