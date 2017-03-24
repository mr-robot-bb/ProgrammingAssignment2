## Calculating the inverse of matrix is hard work
## No worries! with these functions you can calculate the inverse once, and
## retrieve the inverse as often you want!


## Function is used to store a matrix and its inverse
## input: a matrix
## output: a list of getter and setter functions
makeCacheMatrix <- function(x = matrix()) {
    # 'x' is the normal matrix, 'ix' is the Inverse Matrix of x
    ix <- NULL
    # set matrix
    set <- function(y){
       x <<- y
       ix <<- NULL
    }
    # get matrix
    get <- function(){
       x
    }
    # set inverse matrix
    setinverse <- function(inversematrix){
       ix <<- inversematrix
    }
    # get inverse matrix
    getinverse <- function(){
       ix
    }
    # return list of getter and setter functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Function is used to calculate inverse of a matrix when its not already
## calculated. If inverse matrix is already calculated, it returns a cached
## version of the matrix
## input: A list made with the makeCacheMatrix function
## output: A matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    if(!is.null(ix)) {
        message("getting cached data (the inverse matrix)")
        return(ix)
    }
    data <- x$get()
    ix <- solve(data, ...)
    x$setinverse(ix)
    ix
}
