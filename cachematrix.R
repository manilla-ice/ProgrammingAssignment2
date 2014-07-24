## rprog-005
## programming assignment 2 (https://class.coursera.org/rprog-005/human_grading)
## student: Dennis Montenegro ("manilla-ice" on github)
## 07/23/14

makeCacheMatrix <- function(x = matrix()) {

    if ( exists("inverse") ) {
        print("inverse exists")
    } else {
        print("creating inverse")
        inverse <- NULL
    }
    
    set <- function(y) {
        print("set")
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        print("get")
        x
    }
    
    setinverse <- function(inv) {
        print("setinverse")
        inverse <<- inv
        #print(inverse)
        getinverse()
    }
    
    getinverse <- function() {
        print("getinverse")
        inverse
    }
    
    list(
        set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse    

}
