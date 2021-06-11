## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {     ##take x as a matrix vector
        inv <- NULL                             ##initiating inv as a NULL
        set <- function(y) {    ##defining 'set' function
                x <<- y 
                inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(inverse) {inv <<- inverse}   ##defining setinv
        getinv <- function() inv        ## defing getinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) { ## defining 'cacheSolve' function
        inv <- x$getinv()
        if(!is.null(inv)) {     ## checking if inv is NULL or not
                message("getting cached data")
                return(inv)     ## print inv
        }
        data <- x$get()         ## if inv is NULL, caculatte 'inv'
        inv <- solve(data,...)
        x$setinv(inv)
        inv     ##print inv
}
