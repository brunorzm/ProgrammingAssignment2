#The makeCacheMatrix function sets a matrix to a specific environment.
#This function contains a list of functions, such as: 
#'get' that returns a matrix setted before, if there is one.
#'setInverse' that stores the value of the inverse matrix to 'the variable 'inv' using the '<<' operator. So it can be called from other environment
#'getInverse' that returns the value of inv
#'set' that takes a new value of matrix and assign it this environment

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

#The cacheSolve function calls the getInverse above and assigns its value to a variable called 'inv'
#if 'inv' has already been calculated (e.g., it is not null), its value is returned.
#otherwise the get function above is called. So, the value of the matrix defined previously in the environment comes with it
#Then, it will calculate the inverse of this matrix and stores it in the variable inv, returning it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
