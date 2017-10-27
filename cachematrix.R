#Our aim is to write two functions called makeCacheMatrix and cacheSolve that
#caches the inverse of the matrix

#makeCacheMatrix is a function which creates a special "matrix" object that can 
#cache its inverse for the input matrix


makeCacheMatrix <- function(x = matrix()) {
i<- NULL
set<-function(y) {
  x<<-y
  m<<-null
}
get<-function () x
setinverse <-function(inverse) i<<-inverse
getinverse<-function() i
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


#cacheSolve is a function which computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
# then the cacheSolve should retrieve the inverse from the cache, otherwise it
#calculates it

cacheSolve <- function(x, ...) {
        
  
  i<-x$getinverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return (i)
  }
  
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
