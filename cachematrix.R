## These functions take a matrix, and reproduce the original matrix or the inverted version.

## This function takes the matrix as input and produces the inverse, making both the original
##and the inverse available in a list.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if there has already been calculated a inverse and gives you the 
##result if it has already been calculated. If not it will calculate and return it.

cacheSolve <- function(x,...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
        
}
