## The following function sets a very special 'matrix', which is really a list containing a function to do 
## the following things:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function aims to calculate the inverse of the special matrix generated in the above function.
## However, it at the first place checks whether the inverse has already been calculated.
## If so, it with simply get the inverse and skip the repeated computation.
## Otherwise, it will serve to calculate the inverse of the data and set the value of inverse in the cache via setiverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cashed data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

