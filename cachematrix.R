## This pair of functions cache the inverse of a matrix

## this function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  invmtrx<-NULL
  set<-function(y){
    x<<-as.matrix(y)
    invmtrx<<-NULL
  }
  get<-function() x
  setinv<-function(newinvmtrx)invmtrx<<-newinvmtrx
  getinv<-function()invmtrx
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## if the inverse has already been calculated the cachesolve retrieves inverse from the cache 

cacheSolve <- function(xx, ...) {
        data<-matrix()
        invmtrx<-xx$getinv()
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(invmtrx)){
          message("getting cached data")
          return(invmtrx)
        }
        data<-xx$get()
        ##checks if matrix is invertible
        if(det(data)==0){
            print("matrix not invertible")
        }
        else{
        invmtrx<-solve(data)
        xx$setinv(invmtrx)
        invmtrx
        }
}
