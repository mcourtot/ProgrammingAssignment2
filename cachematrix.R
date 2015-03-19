## This is a reponse to the programming assignement 2 from the R programming course at
## https://class.coursera.org/rprog-012/human_grading/view/courses/973493/assessments/3/submissions
## Functions description and stubs have been reused from the course page, and code has been implemented following the examples and syntax provided


## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This pair of functions allow to cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
        invMat <- NULL
        set <- function(y) {
            x <<- y
            invMat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invMat <<- inverse
        getinverse <- function() invMat
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        invMat <- x$getinverse()
        if(!is.null(invMat)) {
            message("Found cached version")
            return(invMat)
        }
        #if no cached version is found
        message("No cached version")
        data <- x$get()
        #compute the inverse of the matrix - note we assume x to be a square invertible matrix
        invMat <- solve(data, ...)
        x$setinverse(invMat)
        invMat
}
