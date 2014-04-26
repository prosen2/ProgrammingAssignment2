## This function will take a matrix as an argument and return the inverse of that matrix
## The function will first check to see if the inverse matrix has already been cached
## If the inverse already exists in the cache, the function will read and return it from the cache
## If the inverse is not in the cache, the function will calculate the inverse and store it
## in the cache. 

## This function creates a special matrix that will allow us to cache the inverse matrix and recall 
## it if the matrix inverse has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL 
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse  #Stores the inverse matrix in cache
  getinverse<-function() m  #retrives the cached matrix
  list(set=set,get=get,setinverse=setinverse, getinverse=getinverse)
}


## This function checks to see if the inverse of the matrix is stored in cache.  If the matrix
## has already been calculated and stored, it will get it from the cache and display it.  If 
## it has not been calculated, it will calculate the inverse, store it in memory, and display it.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
            message("Getting cached data")
            return(m)
        }
        data <-x$get()
        m<-solve(data) %*%data #Creates the inverse matrix
        x$setinverse(m)  #Stores the inverse matrix in cache
        m
}
