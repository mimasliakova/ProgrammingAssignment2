#The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
#The function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above.


makeCacheMatrix <- function(x = matrix()) {
        a <- NULL  #setting the value of the matrix
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x  #getting the value of the matrix
    setinverse <- function(inverse)  #setting the value of the inverse
    a <<- inverse
    getinverse <- function() a  #getting the value of the inverse
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
        a <- x$getinverse()  #computing data returned by makeCacheMatrix
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setinverse(a)
        a
}
