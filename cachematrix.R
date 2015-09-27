## The fist function 'makeCacheMatrix'  creates a special "matrix" object that can cache its inverse. The second funciton 
## 'cacheSolve' computes the inverse of the "matrix" returned by 'makeCacheMatrix'. This funcion is optimized to avoid calculating
## matrix inverse if the matrix has not changed. 


## The function 'makeCacheMatrix' creates a special "matrix" object which contains functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(get = get,set = set,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function 'cacheSolve' takes input as the "matrix" object created by the first function 'makeCacheMatrix' and computes the 
## matrix inverse. This function also avoids computing matrix inverse if matrix hasn't changed. It does so by retrieving cached
## matrix inverse computed previously if original matrix hasn't changed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached matrix inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}