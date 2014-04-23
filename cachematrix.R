## This function will take a matrix as an argument and return the inverse of that matrix
## The function will first check to see if the inverse matrix has already been cached
## If the inverse already exists in the cache, the function will read and return it from the cache
## If the inverse is not in the cache, the function will calculate the inverse and store it
## in the cache. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
            message("Getting cached data")
            return(m)
        }
        data <-x$get()
        m<-solve(data)
        x$setinverse(m)
        m
}
