## Functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        ## Stores a matrix 
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        ## Returns the matrix
        get<-function() x
        
        ## Sets the inverse of the matrix
        setInverse <-function(solve) m<<- solve
        
        ## Gets the inverse of the matrix
        getInverse <-function() m
        
        ## Returns a list of the previous methods
        list(set=set, get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
        ## Returns the inverse if its already set
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## Otherwise
        ## Gets the matrix
        data<-x$get()
        
        ## Calculates the inverse
        m<-solve(data, ...)
        
        ## Sets the inverse
        x$setInverse(m)
        
        ## Returns a matrix that is the inverse of 'x'
        m                        
}
