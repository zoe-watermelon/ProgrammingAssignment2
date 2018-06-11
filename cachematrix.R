## cache the inverse of a matrix

## Create a special "Matrix" object that can cache the input matrix and its inverse
 
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        setmatrix <- function(y){
                x<<-y
                m<<-NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m<<-solve
        getinverse <- function() m
        list(setmatrix=setmatrix,getmatrix=getmatrix,
             setinverse=setinverse,getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                return(m)
        }
        y <- x$getmatrix()
        x$setmatrix(y)
        m <- solve(y,...)
        x$setinverse(m)
        m
}
