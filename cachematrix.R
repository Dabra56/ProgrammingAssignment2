# Setting up the empty matrix, the getters and setter functions
# Only difference from original codes apart from names is the default X value

makeCacheMatrix <- function(x = matrix(,,,)) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
# We are setting the getters and setters of the caching function
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
# Only change is in the mean calculation that is in this case the Solve function
# Since we assume its always invertible, im not testing for it
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}

#Trying out the function for spin
matrix_inv <- matrix(1:4,nrow=2,ncol=2)
cache_matrix <- makeCacheMatrix(matrix_inv)

cacheSolve(cache_matrix)
#Trying out the condition
cacheSolve(cache_matrix)
