## Put comments here that give an overall description of what your
## functions do
##There are two functions,one to create a "special" matrix that can cache the inverse of  matrix
##second one to calculate incerse of matrix and to retrieve the invere from cache if it has already been calculated

## Write a short comment describing this function
## makeCacheMatrix :function creates a special "matrix" object that can cache its inverse
##set         :set the matrix
##get         :get the matrix
##setinverse  :set the inverse of the matrix
##getinverse  :get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
x<<-y
m<<-NULL
}
get<-function() x
setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
##cacheSolve       :function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the ##inverse has already been calculated , then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
