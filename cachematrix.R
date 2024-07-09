# This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversed_matrix <- NULL

        set <- function(y) {
                x <<- y
                inversed_matrix <<- NULL
        }

        get <- function() x
        setMatrix <- function(inverse) inversed_matrix <<- inverse
        getMatrix <- function() inversed_matrix
        list (set = set, get = get,
                setMatrix = setMatrix,
                getMatrix = getMatrix)

}


# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        inversed_matrix <- x$getMatrix()
        if(!is.null(inversed_matrix)) {
                message("getting cached data")
                return(inversed_matrix)
        }
        data <- x$get()
        inversed_matrix <- solve(data, ...)
        x$setMatrix(inversed_matrix)
        inversed_matrix
}

# usage:
# simple_matrix <- matrix(c(4, 7, 2, 6), 2, 2)
# simple_matrix
# cachedMatrix <- makeCacheMatrix(simple_matrix)
# inverse <- cacheSolve(cachedMatrix)
# print(inverse)
