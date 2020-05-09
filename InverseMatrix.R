## Caching the Inverse of a Matrix: Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that stores a matrix
## and caches its inverse. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {	## define function with default mode of "matrix"
        invMat <- NULL					## initialize invMat to NULL to store value of matrix inverse 
        set <- function(y) {				## define the set function to assign new 
                x <<- y					## value of matrix in parent environment
                invMat <<- NULL			## new matrix invMat is set NULL
        }
        get <- function() x							## define the get fucntion that returns value of the matrix argument
        setInverse <- function(inverse) invMat <<- inverse   	## assigns value of invMat in parent environment
        getInverse <- function() invMat				 	## gets the value of invMat 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)## This is used to refer   
 														## the functions with the $ operator
             
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInverse()
        if (!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        mat <- x$get()
        invMat <- solve(mat, ...)
        x$setInverse(invMat)
        invMat
}