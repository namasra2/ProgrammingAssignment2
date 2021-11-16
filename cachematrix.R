## This function creates a special "matrix" object that can cache its inverse

## The function makeCacheMatrix consists of set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL            #initializing inverse as NULL
        set <- function(y){
                          x <<- y
                          inv <<- NULL
                          }
get <- function()x            #this function used to get matrix x 
setinv <- function(inverse)inv<<-inverse
getinv <- function(){
                    inver <- ginv(x)
                    inver%*%x          #this function is used to get the inverse of the matrix
                    }
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## This following function is used to calculate the cache data

cacheSolve <- function(x, ...)    ##gets the cache data
{
inv <- x$getinv()
if(!is.null (inv)){             ##to check whether inverse is null
                   message("calculating cache data")
                   return(inv)       ##returns the inverse value
}
data <- x$get()
inv <- solve(data,...)         ##calculates the inverse value
inv                            ##returns a matrix that is the inverse of "x"
}
