# The first function, makeCacheMatrix creates a special "matrix", which is really 
# an array containing a function to
# 
# set the values of the matrix
# get the values of the matrix
# set the values of the inverse matrix
# get the values of the inverse matrix

# NOTE: I used MASS.ginv because there are opinions that ginv works more reliably
# than solve.

# Example: myTry <- makeCacheMatrix(matrix(c(1,5,7,2,8,3,3,6,4), nrow=3))

require("MASS")
makeCacheMatrix <- function(x = matrix()) {
        g <- NULL
        set <- function(y) {
                x <<- y
                g <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(ginv) g <<- ginv
        getInverseMatrix <- function() g
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

# The following function calculates the inverse of the special "matrix" created with 
# the above function. However, it first checks to see if the inverse matrix has 
#already been calculated. If so, it gets the inverse from the cache and skips the 
#computation. Otherwise, it calculates the inverse of the data and sets the value 
#of the inverse in the cache via the setInverseMatrix function.

cacheSolve <- function(x, ...) {
        g <- x$getInverseMatrix()
        if(!is.null(g)) {
                message("getting cached data")
                return(g)
        }
        data <- x$get()
        g <- solve(data, ...)
        x$setInverseMatrix(g)
        g
}
