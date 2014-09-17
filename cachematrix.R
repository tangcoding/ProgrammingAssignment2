## These two functions combined can calculate the inverse of a matrix and cache it in a matrix object

## create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvs <- function(solve) m <<- solve
        getinvs <- function() m
        list(set = set, get = get,
             setinvs = setinvs,
             getinvs = getinvs)
}


## Calculate the inverse of the special matrix return by makeCacheMatrix function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getinvs()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvs(m)
        m
}
