## makeCacheMartix and cacheSolve work together to increase program
## efficiency by allowing a matrix's inverse to be cached and 
## retrieved

## makeCacheMatrix is a fuction which contains a list of functions 
## which will make the cache for a given inversabile matrix or 
## retrieve it if it has already been made 

makeCacheMatrix <- function(x = matrix()) {

    i<- NULL
    
    set<-function(y) {
        
        x<<- y
        
        i<<- NULL
        
    }
    
    get <-function() x
    
    setinverse <- function(solve) i <<- solve
    
    getinverse <- function() i
    
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    
}


## cacheSolve calls the fuctions stored in makeCacheMatrix to 
##be able to actually produce output  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    if(!is.null(i)) {
        
        message("getting cached data")
      
        return(i)
      
    }
    
    data<-x$get()
    
    i<-solve(data,...)
    
    x$setinverse(i)
    
    i
}
