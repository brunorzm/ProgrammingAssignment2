#The makeCacheMatrix function sets a matrix to a specific environment.
#This function contains a list of functions, such as: 
#'get' that returns a matrix setted before, if there is one.
#'setInverse' that stores the value of the inverse matrix to 'the variable 'm' using the '<<' operator. So it can be called from other environment
#'getInverse' that returns the value of m
#'set' that takes a new value of matrix and assigns it this environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

#The cacheSolve function calls the getInverse above and assigns its value to a variable called 'm'
#if 'm' has already been calculated (e.g., it is not null), its value is returned.
#otherwise the get function above is called. So, the value of the matrix defined previously in the environment comes with it
#Then, it will calculate the inverse of this matrix and stores it in the variable m, returning it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setInverse(m)
        m
}
