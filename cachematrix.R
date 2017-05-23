## The two functions here work together to calculate the inverse of a matrix
## and store it in cache once created.  They need each other to function as
## intended.

## makeCacheMatrix returns a list of 4 named items which include the inverse.
## The input to makeCacheMatrix is a matrix x.
## The output list of makeCacheMatrix is required as input to CacheSolve.


makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setinverse<-function(inversematrix)i<<-inversematrix
        getinverse<-function()i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## CacheSolve returns the inverse of the matrix x, i.
## CacheSolve only solves for the inverse if it is not already in cache.
## CacheSolve is required to populate or retrieve the mean in makeCacheMatrix.

cacheSolve <- function(x, ...) {
                i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
