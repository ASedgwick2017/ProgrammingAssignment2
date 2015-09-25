## makeCacheMartix and cacheSolve work together to increase program
## efficiency by allowing a matrix's inverse to be cached and 
## retrieved

## makeCacheMatrix is a fuction which contains a list of functions 
## which will make the cache for a given inversabile matrix or 
## retrieve it if it has already been made 

makeCacheMatrix <- function(x = matrix()) {

    ##sets the inverse to NULL
    i<- NULL
    
    ##sets the matrix from arguement 
    ##and (re)sets inverse
    set<-function(y) {
        
        x<<- y
        
        i<<- NULL
        
    }
    
    ##gets the matrix
    get <-function() x
    
    ##does the inverse and caches it
    setinverse <- function(solve) i <<- solve
    
    ##recalls the cached inverse
    getinverse <- function() i
    
    ##list of all of the functions
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    
}


## cacheSolve calls the fuctions stored in makeCacheMatrix to 
##be able to actually produce output  

cacheSolve <- function(x, ...) {
        
    ##calls getinverse in makeCacheMatrix
    ##to find the inverse 
    i <- x$getinverse()
    
    ##if the inverse is cached, will get it
    if(!is.null(i)) {
        
        message("getting cached data")
      
        return(i)
      
    }
    
    ##if not already cached this
    ##gets the matrix currently working with
    data<-x$get()
    
    ##solves for the inverse
    i<-solve(data,...)
    
    ##caches the inverse for future use
    x$setinverse(i)
    
    ##returns in the inverse matrix
    i
}
