## Caching Matrix Inverses

# Function takes in a Matrix and computes the 
# inverse using the solve() function. Each 
# unique result is saved to the list.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(solve) m <<- solve
    getMatrix <- function() m
    list(set = set, get = get, 
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


# Checks to see if the matrix passed to 
# makeCacheMatrix() has already been solved.
# If so, return the result stored in the list.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    m
}

