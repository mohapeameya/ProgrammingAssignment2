# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        setmatrix <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        getmatrix <- function() m
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$getmatrix()
        i <- solve(m)
        x$setinverse(i)
        i
}

# Test Result

# > source("cachematrix.R")
# > testmatrix <- matrix(1:4,2,2)
# > testmatrix
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheMatrix <- makeCacheMatrix(testmatrix)
# > cacheMatrix$getmatrix()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheMatrix$getinverse()
# NULL
# > cacheSolve(cacheMatrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(cacheMatrix)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 