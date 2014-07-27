
## course <- "rprog-005"
## student <- "Dennis Montenegro ('manilla-ice' on github)"
## assignment <- "programming assignment 2 (https://class.coursera.org/rprog-005/human_grading)"
## dates.sumbitted <- c("07/24/14", "07/27/2014")



## Reference materials for matrix inversion:
##   http://www.mathsisfun.com/algebra/matrix-inverse.html
## Based on the above link, here are the raw matrix and its inverse in the R console.
##   > mtx <- matrix(c(4,2,7,6), nrow=2, ncol=2)
##   > mtx
##        [,1] [,2]
##   [1,]    4    7
##   [2,]    2    6
##   > solve(mtx)
##        [,1] [,2]
##   [1,]  0.6 -0.7
##   [2,] -0.2  0.4



## The "makeCacheMatrix" function is the "object" for the raw matrix("x") and its inverse.
## The input param is the raw matrix "x"
## The function starts off by initializing the inverse variable if it does not already exist.
## The sub-functions act as accessors ("get) and mutators ("set") for both x and its inverse.  
## Note how they use the "<<-" assignment operator, which traverses higher environments to assign values to variables.
## The function ends with returning the sub-functions as a list.
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


## The "cacheSolve" function uses the "makeCacheMatrix" function to efficiently solve for an inverse.
## Its input param should use the "makeCacheMatrix" function.  Using the example in the reference materials above, 
##   > cacheSolve(makeCacheMatrix(mtx))
## As part of its processing, the "cacheSolve" function will use a cached value of the inverse if available.
## The output will be the inverse matrix, as well as indications of whether or not it took the inverse from cache.
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
