## This function creates a special "matrix" object that can 
##cache its inverse. The input is matrix x, and this function 
## returns a "matrix" of functions to cache and retrieve matrix x, 
## and retrieve its inverse from cache

makeCacheMatrix <- function(x = matrix()) {
    
    #initialize inverse of matrix 
    xinverse<-matrix()
    
    #cache matrix and reset the inverse of matrix to be NA
    set<- function(y) {
        x<<-y
        xinverse<<-matrix()
    }
    #retrieve matrix
    getmatrix<-function() x
    
    #cache inverse
    setinverse<-function(inverse) xinverse<<-inverse
    
    #retrieve inverse
    getinverse<-function() xinverse
    
    #returns a matrix of functions to cache and retrieve matrix, 
    # and retrieve its inverse from cache
    list(set=set, getmatrix=getmatrix, setinverse=setinverse, 
         getinverse=getinverse)
}


## This function computes the inverse of the special matrix 
## returned by makeCacheMatrix function. This function
## returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
    xinverse<- x$getinverse() #get inverse
    
    #if cached value for inverse has been calculated  
    # (not NA), return the cached value
    if(!is.na(xinverse)) {
        message("getting cached matrix inverse")
        return(xinverse)
    }
    #if inverse is NA (has not been calculated yet)
    else {
        #get cached matrix, and calculate matrix inverse
        data<- x$getmatrix()
        xinverse<- solve(data)
        x$setinverse(xinverse) #cache inverse
    }
    xinverse
}
